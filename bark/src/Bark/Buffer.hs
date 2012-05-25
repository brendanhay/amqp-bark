{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, KindSignatures,
    RankNTypes #-}

module Bark.Buffer (
    -- * Exported Types
      Buffer()

    -- * Constructors
    , newUnbounded
    , newOverflow

    -- * Conduits
    , sourceBuffer
    , sinkBuffer

    -- * Buffer Operations
    , revertBuffer

    -- * STM
    , liftSTM
    , atomically
    ) where

import Control.Monad                  (liftM, unless)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.STM              (STM, atomically)
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TBMChan
import Data.Conduit
import Data.Typeable                  (Typeable)

data Buffer a = Unbounded (TMChan a) | Overflow (TBMChan a) deriving (Typeable)

newUnbounded :: STM (Buffer a)
newUnbounded = liftM Unbounded newTMChan

newOverflow :: Int -> STM (Buffer a)
newOverflow = liftM Overflow . newTBMChan

sourceBuffer :: MonadIO m => Buffer a -> Source m a
sourceBuffer buf =
      source
    where
      source = PipeM pull close
      pull   = do
           a <- liftSTM $ readBuffer buf
           case a of
               Nothing -> return $ Done Nothing ()
               Just x  -> return $ HaveOutput source close x
      close  = liftSTM $ closeBuffer buf

sinkBuffer :: MonadIO m => Buffer a -> Sink a m ()
sinkBuffer buf =
      sink
    where
      sink       = NeedInput push (liftSTM $ closeBuffer buf)
      push input = PipeM (liftSTM $ writeBuffer buf input >> return sink)
                         (liftSTM $ closeBuffer buf)

revertBuffer :: Buffer a -> a -> STM ()
revertBuffer (Unbounded chan) val = unGetTMChan chan val
revertBuffer (Overflow chan) val  = do
    closed <- isClosedTBMChan chan
    unless closed $ do
        slots <- estimateFreeSlotsTBMChan chan
        unless (slots <= 0) $ unGetTBMChan chan val

liftSTM :: forall (m :: * -> *) a. MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

--
-- Internal
--

readBuffer :: Buffer a -> STM (Maybe a)
readBuffer (Unbounded chan) = readTMChan chan
readBuffer (Overflow chan)  = readTBMChan chan

writeBuffer :: Buffer a -> a -> STM ()
writeBuffer (Unbounded chan) val    = writeTMChan chan val
writeBuffer buf@(Overflow chan) val = do
    closed <- isClosedTBMChan chan
    unless closed $ do
        slots <- estimateFreeSlotsTBMChan chan
        if slots <= 0
            then tryReadTBMChan chan >> writeBuffer buf val
            else writeTBMChan chan val

closeBuffer :: Buffer a -> STM ()
closeBuffer (Unbounded chan) = closeTMChan chan
closeBuffer (Overflow chan)  = closeTBMChan chan
