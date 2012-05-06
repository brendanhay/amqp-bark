{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit
import Data.Conduit.Binary    (sourceHandle, sinkHandle)
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import Data.Word              (Word8)
import Data.Version           (Version(..), versionBranch)
import Network.AMQP.Conduit
import System.IO              (stdin, stdout)

import qualified Data.ByteString as BS

class Delimiter a where
    split   :: a -> BS.ByteString -> (BS.ByteString, BS.ByteString)
    strip   :: a -> BS.ByteString -> BS.ByteString

instance Delimiter Word8 where
    split   = BS.breakByte
    strip _ = id

instance Delimiter BS.ByteString where
    split       = BS.breakSubstring
    strip delim = BS.drop (BS.length delim)

delimitConduit :: Monad m
               => Delimiter d
               -> Conduit BS.ByteString m BS.ByteString
delimitConduit delim =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        buffer        = BS.append state input
        (match, rest) = split delim buffer
        (state', res) | BS.null rest  = (buffer, [])
                      | BS.null match = (buffer, [])
                      | otherwise     = (strip delim rest, [match])

    close state = return [state]

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = []
    }

bufferSize :: Int
bufferSize = 32

main :: IO ()
main = do
    chan <- atomically $ newTBMChan bufferSize
    _    <- forkIO . runResourceT
                         $  sourceHandle stdin
                         $$ sinkTBMChan chan
    runResourceT
        $   sourceTBMChan chan
        $=  delimitConduit ("--" :: BS.ByteString)
        =$= amqpConduit uri exchange queue
        $$  sinkHandle stdout
  where
     uri      = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
     exchange = newExchange { exchangeName = "test", exchangeType = "direct" }
     queue    = newQueue { queueName = "test" }