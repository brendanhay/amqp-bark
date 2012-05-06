{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8
import Data.Conduit
import Data.Conduit.Binary    (sourceHandle, sinkHandle)
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import Data.Word              (Word8)
import Data.Version           (Version(..), versionBranch)
import Network.AMQP.Conduit
import System.IO              (stdin, stdout)

import qualified Data.ByteString as BS

type Strategy = Monad m => Conduit BS.ByteString m BS.ByteString

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = []
    }

bound :: Int
bound = 32

strategy :: BS.ByteString -> Strategy
strategy delim | (BS.length delim) > 1 = byString delim
               | otherwise             = byByte $ BS.head delim

byByte :: Word8 -> Strategy
byByte word =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        buffer        = BS.append state input
        (match, rest) = BS.breakByte word buffer

        (state', res) | BS.null rest  = (buffer, [])
                      | BS.null match = (buffer, [])
                      | otherwise     = (rest, [match])

    close state = return [state]

byString :: BS.ByteString -> Strategy
byString bstr =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        buffer        = BS.append state input
        strip         = BS.drop (BS.length bstr)
        (match, rest) = BS.breakSubstring bstr buffer

        (state', res) | BS.null rest  = (buffer, [])
                      | BS.null match = (buffer, [])
                      | otherwise     = (strip rest, [match])

    close state = return [state]

main :: IO ()
main = do
    chan <- atomically $ newTBMChan bound
    _    <- forkIO . runResourceT
                         $  sourceHandle stdin
                         $$ sinkTBMChan chan
    runResourceT
        $   sourceTBMChan chan
        $=  strategy "--"
        =$= amqpConduit uri exchange queue
        $$  sinkHandle stdout
  where
     uri      = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
     exchange = newExchange { exchangeName = "test", exchangeType = "direct" }
     queue    = newQueue { queueName = "test" }