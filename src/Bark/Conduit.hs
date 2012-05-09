{-# LANGUAGE ExistentialQuantification #-}

module Bark.Conduit (
      Delimiter(..)
    , fromString
    , conduitSplit
    , conduitHandle
    ) where

import Control.Monad.IO.Class   (liftIO)
import Data.ByteString.Char8    (pack)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Unsafe   (unsafeTake, unsafeDrop, unsafeTail)
import Data.Conduit
import Data.Word                (Word8)
import System.IO                (Handle)

import qualified Data.ByteString as BS

data AnyDelimiter = forall a. Delimiter a => AnyDelimiter a

class Delimiter a where
    split :: a -> BS.ByteString -> (BS.ByteString, BS.ByteString)
    strip :: a -> BS.ByteString -> BS.ByteString

instance Delimiter AnyDelimiter where
    split (AnyDelimiter d) = split d
    strip (AnyDelimiter d) = strip d

instance Delimiter Word8 where
    split        = BS.breakByte
    strip _ bstr = BS.take ((BS.length bstr) - 1) bstr

instance Delimiter BS.ByteString where
    split d bstr = breakSubstring d bstr False
    strip d bstr = BS.take ((BS.length bstr) - (BS.length d)) bstr

-- combined strip/split
-- "abc--defg"
-- rest: drop index + (length delim)
-- match: take index, optionally + (length delim) if include delims

fromString :: String -> AnyDelimiter
fromString str | (length str) > 1 = AnyDelimiter $ pack str
               | otherwise        = AnyDelimiter . c2w $ head str

conduitSplit :: (Delimiter d, Monad m)
             => d
             -> Conduit BS.ByteString m BS.ByteString
conduitSplit delim =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        (state', res) | BS.null rest  = (buffer, [])
                      | BS.null match = (buffer, [])
                      | otherwise     = (rest, [match])
          where
            buffer        = BS.append state input
            (match, rest) = split delim buffer
    close state = return [state]

conduitHandle :: MonadResource m
              => Handle
              -> Conduit BS.ByteString m BS.ByteString
conduitHandle handle =
    conduit
  where
    conduit = NeedInput push close
    push bstr = PipeM
        (liftIO (BS.hPut handle bstr) >> return conduit)
        (return ())
    close = return ()

--
-- Internal
--

breakSubstring :: BS.ByteString
               -> BS.ByteString
               -> Bool
               -> (BS.ByteString, BS.ByteString)
breakSubstring pat bstr drop =
    search 0 bstr
  where
    len = BS.length pat
    pad = if drop then 0 else len

    search a b | a `seq` b `seq` False = undefined
    search n s | BS.null s             = (BS.empty, bstr)
               | pat `BS.isPrefixOf` s = (BS.take (n + pad) bstr, BS.drop len s)
               | otherwise             = search (n + 1) (unsafeTail s)