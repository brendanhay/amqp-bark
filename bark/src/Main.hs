{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts,
    RankNTypes #-}

module Main
    ( main
    ) where

import Control.Concurrent          (forkIO, threadDelay)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.Conduit
import System.IO                   (stdin)
import Bark.AMQP
import Bark.Buffer
import Bark.IO
import Bark.Options
import Bark.Types


import qualified Data.ByteString.Char8  as B
import qualified Bark.Event.Exact       as E
import qualified Bark.Event.Incremental as I

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts

    raw    <- atomically $ newBuffer optBound
    events <- atomically $ newBuffer optBound

    _ <- forkIO . runResourceT $ sinkEvents opts events
    _ <- forkIO . runResourceT $ conduitParser opts raw events

    runResourceT $ sourceStdin opts raw

--
-- Internal
--

sourceStdin :: (MonadBaseControl IO m, MonadResource m)
            => Options
            -> Buffer B.ByteString
            -> m ()
sourceStdin Options{..} output =
    sourceHandle stdin optBuffer $$ sinkBuffer output

conduitParser :: (MonadBaseControl IO m, MonadResource m)
              => Options
              -> Buffer B.ByteString
              -> Buffer Event
              -> m ()
conduitParser Options{..} input output =
    sourceBuffer input $= tee (parser optDelim optStrip) $$ sinkBuffer output
  where
    tee | optTee    = (=$= conduitShow)
        | otherwise = id
    parser = case optParser of
        Exact       -> E.conduitEvent
        Incremental -> I.conduitEvent

sinkEvents :: MonadIO m
           => Options
           -> Buffer Event
           -> m ()
sinkEvents Options{..} input =
    sink $ sourceBuffer input
  where
    sink src = do
        conn        <- alloc
        (src', res) <- src $$+ sinkAMQP conn
        case res of
            Nothing -> return ()
            Just (PublishFailure evt) -> do
                liftIO $ disconnect conn
                liftSTM $ revertBuffer input evt
                sink src'

    alloc = attempt >>= either failure return
      where
        attempt    = liftTry $ connect optUri optHost optService
        failure ex = do
            liftIO $ do
                print ex
                threadDelay 600000
            alloc