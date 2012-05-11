{-# LANGUAGE ExistentialQuantification #-}

module Bark.Binary (
      Delimiter(..)
    , fromString
    , sourceHandle
    , conduitSplit
    , conduitHandle
    ) where

import Control.Monad.IO.Class   (liftIO)
import Data.ByteString.Char8    (pack)
import Data.ByteString.Internal (ByteString(PS), c2w, memchr, inlinePerformIO)
import Data.ByteString.Unsafe   (unsafeTake, unsafeDrop, unsafeTail)
import Data.Conduit
import Data.Word                (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO                (Handle)

import qualified Data.ByteString as BS

data AnyDelimiter = forall a. Delimiter a => AnyDelimiter a

class Delimiter a where
    split :: a -> Bool -> BS.ByteString -> ([BS.ByteString], BS.ByteString)

instance Delimiter AnyDelimiter where
    split (AnyDelimiter d) = split d

instance Delimiter Word8 where
    split = breakByte

instance Delimiter BS.ByteString where
    split = breakString

fromString :: String -> AnyDelimiter
fromString str | (length str) > 1 = AnyDelimiter $ pack str
               | otherwise        = AnyDelimiter . c2w $ head str

sourceHandle :: MonadResource m
             => Handle
             -> Int
             -> Source m BS.ByteString
sourceHandle handle buffer =
    source
  where
    source = PipeM pull close
    pull = do
        bstr <- liftIO (BS.hGetSome handle buffer)
        if BS.null bstr
            then return $ Done Nothing ()
            else return $ HaveOutput source close bstr
    close = return ()

conduitSplit :: (Delimiter d, Monad m)
             => d
             -> Bool
             -> Conduit BS.ByteString m BS.ByteString
conduitSplit delim strip =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        (state', res) | null matches = (buffer, [])
                      | otherwise    = (rest, matches)
          where
            buffer          = BS.append state input
            (matches, rest) = split delim strip buffer
    close state = return [state]

conduitHandle :: MonadResource m
              => Handle
              -> Conduit BS.ByteString m BS.ByteString
conduitHandle handle =
    conduitIO (return handle) (\_ -> return ()) push (const $ return [])
  where
    push h input = do
        liftIO $ BS.hPut h input
        return $ IOProducing [input]

--
-- Internal
--

breakByte :: Word8
          -> Bool
          -> BS.ByteString
          -> ([BS.ByteString], BS.ByteString)
breakByte d strip bstr@(PS x s l) | l == 0    = ([], bstr)
                                 | otherwise = formatResult $ search 0
    where
        search a | a `seq` False = undefined
        search n =
            let q = inlinePerformIO $ withForeignPtr x $ \p ->
                      memchr (p `plusPtr` (s + n)) d (fromIntegral (l - n))
            in if q == nullPtr
                then [PS x (s + n) (l - n)]
                else let i = inlinePerformIO $ withForeignPtr x $ \p ->
                               return (q `minusPtr` (p `plusPtr` s))
                      in if i - n == 0
                          then search (i + 1)
                          else PS x (s + n) (i - n + plen) : search (i + 1)
                            where
                             plen = if strip then 0 else 1

breakString :: BS.ByteString
            -> Bool
            -> BS.ByteString
            -> ([BS.ByteString], BS.ByteString)
breakString d strip bstr | BS.null d = ([], bstr)
                        | otherwise = formatResult $ search 0 bstr 0
  where
    search a b c | a `seq` b `seq` c `seq` False = undefined
    search n s p | BS.null s           = [unsafeDrop p bstr]
                 | d `BS.isPrefixOf` s = cons $ search plen (unsafeDrop dlen s) plen
                 | otherwise           = search (n + 1) (unsafeTail s) p
      where
        dlen  = BS.length d -- Delimiter length
        plen  = n + dlen    -- Distance to move the search ptr
        slen  = (if strip then n else plen) - p -- Slice length relative to previous slice

        slice = unsafeTake slen $ unsafeDrop p bstr

        cons | slen == dlen  = id -- Lonely delimiter
             | BS.null slice = id -- Empty slice
             | otherwise     = (slice :)

formatResult :: [BS.ByteString] -> ([BS.ByteString], BS.ByteString)
formatResult l = case l of
    []  -> ([], BS.empty)
    [x] -> ([], x)
    _   -> (init l, last l)