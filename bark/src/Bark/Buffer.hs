{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, KindSignatures,
    RankNTypes #-}

module Bark.Buffer (
    -- * Exported Types
      Buffer()

    -- * Constructors
    , newBounded
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

import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.STM              (STM, atomically, retry)
import Control.Concurrent.STM.TBMChan
import Data.Conduit
import Data.Typeable                  (Typeable)

data Buffer a = Buffer Bool (TBMChan a) deriving (Typeable)

newBounded :: Int -> STM (Buffer a)
newBounded n = newTBMChan n >>= return . Buffer False

newOverflow :: Int -> STM (Buffer a)
newOverflow n = newTBMChan n >>= return . Buffer True

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
revertBuffer (Buffer _block chan) val = do
    closed <- isClosedTBMChan chan
    if closed
        then return ()
        else do
            slots <- estimateFreeSlotsTBMChan chan
            if slots <= 0
                then return ()
                else unGetTBMChan chan val

liftSTM :: forall (m :: * -> *) a. MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

--
-- Internal
--

readBuffer :: Buffer a -> STM (Maybe a)
readBuffer (Buffer _block chan) = readTBMChan chan

writeBuffer :: Buffer a -> a -> STM ()
writeBuffer buf@(Buffer block chan) val = do
    closed <- isClosedTBMChan chan
    if closed
        then return ()
        else do
            slots <- estimateFreeSlotsTBMChan chan
            if slots <= 0
                then flush
                else writeTBMChan chan val
  where
    flush | block     = tryReadTBMChan chan >> writeBuffer buf val
          | otherwise = retry

closeBuffer :: Buffer a -> STM ()
closeBuffer (Buffer _block chan) = closeTBMChan chan
