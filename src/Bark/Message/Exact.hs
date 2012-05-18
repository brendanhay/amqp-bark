{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module Bark.Message.Exact
    ( conduitMessage
    ) where

import Data.Attoparsec
import Data.ByteString.Char8    (pack)
import Data.ByteString.Internal (ByteString(PS), c2w, memchr, inlinePerformIO)
import Data.ByteString.Unsafe   (unsafeTake, unsafeDrop, unsafeTail)
import Data.Conduit
import Data.Monoid              (mempty)
import Data.Word                (Word8)
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr              (nullPtr, plusPtr, minusPtr)
import Bark.Message.Types
import Bark.Message.Parser

import qualified Data.ByteString as B

conduitMessage :: MonadResource m
               => String
               -> Bool
               -> Conduit B.ByteString m Message
conduitMessage delim strip =
    conduitSplit (fromString delim) strip =$= conduit
  where
    msg input = case parseOnly parser input of
        Right m -> m
        Left  e -> Message "error" "error" . Error $ pack e
    conduit = NeedInput push mempty
    push    = HaveOutput conduit (return ()) . msg

--
-- Internal
--

data AnyDelimiter = forall a. Delimiter a => AnyDelimiter a

class Delimiter a where
    split :: a -> Bool -> B.ByteString -> ([B.ByteString], B.ByteString)

instance Delimiter AnyDelimiter where
    split (AnyDelimiter d) = split d

instance Delimiter Word8 where
    split = breakByte

instance Delimiter B.ByteString where
    split = breakString

fromString :: String -> AnyDelimiter
fromString str | length str > 1 = AnyDelimiter $ pack str
               | otherwise      = AnyDelimiter . c2w $ head str

parser :: Parser Message
parser = do
    sev  <- severity
    cat  <- category
    body <- takeByteString
    return $! Message sev cat (Payload body)

conduitSplit :: (Delimiter d, Monad m)
             => d
             -> Bool
             -> Conduit B.ByteString m B.ByteString
conduitSplit delim strip =
    conduitState B.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        (state', res) | null matches = (buffer, [])
                      | otherwise    = (rest, matches)
          where
            buffer          = B.append state input
            (matches, rest) = split delim strip buffer
    close state = return [state]

breakByte :: Word8
          -> Bool
          -> B.ByteString
          -> ([B.ByteString], B.ByteString)
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

breakString :: B.ByteString
            -> Bool
            -> B.ByteString
            -> ([B.ByteString], B.ByteString)
breakString d strip bstr | B.null d = ([], bstr)
                        | otherwise = formatResult $ search 0 bstr 0
  where
    search a b c | a `seq` b `seq` c `seq` False = undefined
    search n s p | B.null s           = [unsafeDrop p bstr]
                 | d `B.isPrefixOf` s = cons $ search plen (unsafeDrop dlen s) plen
                 | otherwise           = search (n + 1) (unsafeTail s) p
      where
        dlen  = B.length d -- Delimiter length
        plen  = n + dlen    -- Distance to move the search ptr
        slen  = (if strip then n else plen) - p -- Slice length relative to previous slice

        slice = unsafeTake slen $ unsafeDrop p bstr

        cons | slen == dlen  = id -- Lonely delimiter
             | B.null slice = id -- Empty slice
             | otherwise     = (slice :)

formatResult :: [B.ByteString] -> ([B.ByteString], B.ByteString)
formatResult l = case l of
    []  -> ([], B.empty)
    [x] -> ([], x)
    _   -> (init l, last l)