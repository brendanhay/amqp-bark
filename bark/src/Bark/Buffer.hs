{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, KindSignatures, RankNTypes #-}

module Bark.Buffer (
    -- * Exported Types
      Buffer()

    -- * Conduits
    , sourceBuffer
    , sinkBuffer

    -- * Buffer Operations
    , newBuffer
    , revertBuffer

    -- * STM
    , liftSTM
    , atomically
    ) where

import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.STM              (STM, atomically)
import Control.Concurrent.STM.TBMChan
import Data.Conduit
import Data.Typeable                  (Typeable)

data Buffer a = Buffer (TBMChan a) deriving (Typeable)

sourceBuffer :: MonadIO m => Buffer a -> Source m a
sourceBuffer buf = source
    where
        source = PipeM pull close
        pull   = do
             a <- liftSTM $ readBuffer buf
             case a of
                 Nothing -> return $ Done Nothing ()
                 Just x  -> return $ HaveOutput source close x
        close  = liftSTM $ closeBuffer buf

sinkBuffer :: MonadIO m => Buffer a -> Sink a m ()
sinkBuffer buf = sink
    where
      sink       = NeedInput push close
      push input = PipeM (liftSTM $ writeBuffer buf input >> return sink) (liftSTM $ closeBuffer buf)


      close      = liftSTM $ closeBuffer buf

newBuffer :: Int -> STM (Buffer a)
newBuffer n = newTBMChan n >>= return . Buffer

revertBuffer :: Buffer a -> a -> STM ()
revertBuffer (Buffer chan) val = do
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
readBuffer (Buffer chan) = readTBMChan chan

writeBuffer :: Buffer a -> a -> STM ()
writeBuffer buf@(Buffer chan) val = do
    closed <- isClosedTBMChan chan
    if closed
        then return ()
        else do
            slots <- estimateFreeSlotsTBMChan chan
            if slots <= 0
                then tryReadTBMChan chan >> writeBuffer buf val
                else writeTBMChan chan val

closeBuffer :: Buffer a -> STM ()
closeBuffer (Buffer chan) = closeTBMChan chan
