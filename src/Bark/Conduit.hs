module Bark.Conduit (
      Delimiter
    , splitConduit
    ) where

import Data.Conduit
import Data.Word (Word8)

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

splitConduit :: (Delimiter d, Monad m)
             => d
             -> Conduit BS.ByteString m BS.ByteString
splitConduit delim =
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
