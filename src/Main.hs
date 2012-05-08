{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (liftIO)
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

conduits :: MonadResource m
         => Options
         -> Conduit BS.ByteString m BS.ByteString
conduits opts@Options{..} =
    (selectSplit $ delimiter opts) =$= (conduitHandle stdout)

sink :: TBMChan BS.ByteString -> Options -> IO ()
sink chan opts =
    runResourceT
        $  sourceTBMChan chan
        $= conduits opts
--        $$ sinkAMQP uri exchange queue
        $$ sinkHandle stdout
  where
    uri       = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
    exchange  = newExchange { exchangeName = "test", exchangeType = "direct" }
    queue     = newQueue { queueName = "test" }

main :: IO ()
main = do
    opts@Options{..} <- parseOptions

    putStrLn $ show $ delimiter opts
    putStrLn $ show opts

    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ source chan
    sink chan opts