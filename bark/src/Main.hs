{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts,
    RankNTypes #-}

module Main
    ( main
    ) where

import Control.Concurrent          (forkIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.STM           (atomically)
import Data.Conduit
import Data.Conduit.TMChan
import System.IO                   (stdin)
import Bark.AMQP
import Bark.Conduit
import Bark.Options
import Bark.Types

import qualified Data.ByteString.Char8  as B
import qualified Bark.Event.Exact       as E
import qualified Bark.Event.Incremental as I

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts

    raw    <- atomically $ newTBMChan optBound
    events <- atomically $ newTBMChan optBound

    _ <- forkIO . runResourceT $ sinkEvents opts events
    _ <- forkIO . runResourceT $ conduitParser opts raw events

    runResourceT $ sourceStdin opts raw

--
-- Internal
--

sourceStdin :: (MonadBaseControl IO m, MonadResource m)
            => Options
            -> TBMChan B.ByteString
            -> m ()
sourceStdin Options{..} output =
    sourceHandle stdin optBuffer $$ sinkTBMChan output

conduitParser :: (MonadBaseControl IO m, MonadResource m)
             => Options
             -> TBMChan B.ByteString
             -> TBMChan Event
             -> m ()
conduitParser Options{..} input output =
    sourceTBMChan input $= tee (parser optDelim optStrip) $$ sinkTBMChan output
  where
    tee | optTee    = (=$= conduitShow)
        | otherwise = id
    parser = case optParser of
        Exact       -> E.conduitEvent
        Incremental -> I.conduitEvent

sinkEvents :: (MonadBaseControl IO m, MonadResource m)
           => Options
           -> TBMChan Event
           -> m ()
sinkEvents Options{..} input =
    run $ sourceTBMChan input
  where
    run src = do
        (src', ex) <- src $$+ sinkAMQP optUri optHost optService
        case ex of
            Nothing -> do
                liftIO $ putStrLn "Done"
                return ()
            Just (PublishFailure evt) -> do
                liftIO . atomically $ unGetTBMChan input evt
                liftIO . putStrLn $ "Failed " ++ show evt
                run src'

