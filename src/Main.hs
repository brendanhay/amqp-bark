{-# LANGUAGE OverloadedStrings #-}

module Main (
      main
    , conduits
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit
import Data.Conduit.Binary    (sourceHandle, sinkHandle)
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import Network.AMQP.Conduit
import System.IO              (stdin, stdout)
import Bark.Conduit
import Bark.Options

import qualified Data.ByteString as BS

source :: TBMChan BS.ByteString -> IO ()
source chan =
     runResourceT
         $  sourceHandle stdin
         $$ sinkTBMChan chan

conduits :: MonadResource m => Conduit BS.ByteString m BS.ByteString
conduits =
    splitConduit delimiter =$= amqpConduit uri exchange queue
  where
    delimiter = "--" :: BS.ByteString
    uri       = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
    exchange  = newExchange { exchangeName = "test", exchangeType = "direct" }
    queue     = newQueue { queueName = "test" }

sink :: TBMChan BS.ByteString -> IO ()
sink chan =
    runResourceT
        $  sourceTBMChan chan
        $= conduits
        $$ sinkHandle stdout

main :: IO ()
main = do
    chan <- atomically $ newTBMChan 32
    _    <- forkIO $ source chan
    sink chan