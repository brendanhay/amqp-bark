{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts,
    RankNTypes #-}

module Main
    ( main
    ) where

import Control.Concurrent          (forkIO)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.Conduit
import System.IO                   (stdin)
import Bark.AMQP
import Bark.Buffer
import Bark.IO
import Bark.Options
import Bark.Parser
import Bark.Types

import qualified Data.ByteString.Char8  as B

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts

    raw    <- atomically newUnbounded
    events <- atomically $ newOverflow optDeliverBuffer

    _ <- forkIO . runResourceT $ sinkEvents opts events
    _ <- forkIO . runResourceT $ conduitParser opts raw events

    runResourceT $ sourceStdin opts raw

--
-- Internal
--

sourceStdin :: (MonadResource m, MonadIO m)
            => Options
            -> Unbounded B.ByteString
            -> m ()
sourceStdin Options{..} output =
    sourceHandle stdin optReadBytes $$ sinkBuffer output

conduitParser :: (MonadResource m, MonadIO m)
              => Options
              -> Unbounded B.ByteString
              -> Overflow Event
              -> m ()
conduitParser Options{..} input output =
    sourceBuffer input $= tee (conduitEvent optDelim optStrip) $$ sinkBuffer output
  where
    tee | optTee    = (=$= conduitShow)
        | otherwise = id

sinkEvents :: MonadIO m
           => Options
           -> Overflow Event
           -> m ()
sinkEvents Options{..} input =
    sink $ sourceBuffer input
  where
    sink src = do
        conn        <- liftIO $ connect optUri optReconnect optHost optService
        (src', res) <- src $$+ sinkAMQP conn
        case res of
            Nothing -> return ()
            Just (PublishFailure evt) -> do
                liftIO $ disconnect conn
                liftSTM $ revertBuffer input evt
                sink src'
