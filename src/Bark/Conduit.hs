{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bark.Conduit (
      Delimiter(..)
    , selectSplit
    , conduitSplit
    , conduitHandle
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (c2w)
import Data.Conduit
import Data.Word (Word8)
import System.IO (Handle)

import qualified Data.ByteString as BS

class Delimiter a where
    split :: a -> BS.ByteString -> (BS.ByteString, BS.ByteString)
    strip :: a -> BS.ByteString -> BS.ByteString

instance Delimiter Word8 where
    split   = BS.breakByte
    strip _ = id

instance Delimiter BS.ByteString where
    split       = BS.breakSubstring
    strip delim = BS.drop (BS.length delim)

-- instance Delimiter (Either BS.ByteString Word8) where
--     split = split . select
--     strip = strip . select

-- fromString :: String -> Either BS.ByteString Word8
-- fromString str | (length str) > 1 = Left $ pack str
--                | otherwise        = Right $ c2w $ head str

fromString :: Delimiter d => String -> d
fromString str | (length str) > 1 = pack str
               | otherwise        = c2w $ head str

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
                      | otherwise     = (strip delim rest, [match])
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