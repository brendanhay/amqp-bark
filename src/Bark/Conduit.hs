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
import Data.ByteString.Unsafe   (unsafeTake, unsafeDrop)
import Data.Conduit
import Data.Word                (Word8)
import System.IO                (Handle)

import qualified Data.ByteString as BS

class Delimiter a where
    split :: a -> BS.ByteString -> (BS.ByteString, BS.ByteString)

data AnyDelimiter = forall a. Delimiter a => AnyDelimiter a

instance Delimiter AnyDelimiter where
    split (AnyDelimiter d) = split d

instance Delimiter Word8 where
    split = breakIndices . BS.elemIndices
    {-# INLINE split #-}

instance Delimiter BS.ByteString where
    split = breakIndices . BS.findSubstrings
    {-# INLINE split #-}

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

breakIndices :: (BS.ByteString -> [Int])
             -> BS.ByteString
             -> (BS.ByteString, BS.ByteString)
breakIndices indices bstr = case indices bstr of
    []    -> (BS.empty, bstr)
    [0]   -> (bstr, BS.empty)
    0:n:_ -> (unsafeTake n bstr, unsafeDrop n bstr)
    n:_   -> (unsafeTake n bstr, unsafeDrop n bstr)
