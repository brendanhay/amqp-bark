{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Conduit.Binary    (sinkHandle)
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import Network.AMQP.Conduit
import System.IO              (stdin, stdout)
import Bark.Conduit
import Bark.Options

import Data.ByteString.Char8    (pack)
import Data.ByteString.Internal (c2w)

import qualified Data.ByteString as BS

source :: TBMChan BS.ByteString -> Options -> IO ()
source chan Options{..} =
     runResourceT
         $  sourceHandle stdin optBuffer
         $$ sinkTBMChan chan

conduits :: MonadResource m
         => Options
         -> Conduit BS.ByteString m BS.ByteString
conduits Options{..} = conduitSplit (fromString optDelimiter) optStrip

sink :: TBMChan BS.ByteString -> Options -> IO ()
sink chan opts =
    runResourceT
        $  sourceTBMChan chan
        $= conduits opts
--        $$ sinkAMQP uri exchange queue
        $$ sinkHandle stdout
  where
    uri      = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
    exchange = newExchange { exchangeName = "test", exchangeType = "direct" }
    queue    = newQueue { queueName = "test" }

main :: IO ()
main = do
    opts@Options{..} <- parseOptions

    putStrLn $ show opts
    print $ c2w $ head optDelimiter
    print $ pack optDelimiter

    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ source chan opts
    sink chan opts