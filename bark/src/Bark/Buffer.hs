{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, KindSignatures,
    RankNTypes, TypeSynonymInstances #-}

module Bark.Buffer (
    -- * Exported Types
      Buffer(..)
    , Unbounded
    , Overflow

    -- * Constructors
    , newUnbounded
    , newOverflow

    -- * Conduits
    , sourceBuffer
    , sinkBuffer

    -- * STM
    , liftSTM
    , atomically
    ) where

import Control.Monad                  (unless)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.STM              (STM, atomically)
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TBMChan
import Data.Conduit

type Unbounded = TMChan
type Overflow  = TBMChan

class Buffer b where
    readBuffer   :: b a -> STM (Maybe a)
    writeBuffer  :: b a -> a -> STM ()
    revertBuffer :: b a -> a -> STM ()
    closeBuffer  :: b a -> STM ()

instance Buffer TMChan where
    readBuffer   = readTMChan
    writeBuffer  = writeTMChan
    revertBuffer = unGetTMChan
    closeBuffer  = closeTMChan

instance Buffer TBMChan where
    readBuffer = readTBMChan

    writeBuffer c v = do
        p <- isClosedTBMChan c
        unless p $ do
            n <- estimateFreeSlotsTBMChan c
            if n <= 0
                then tryReadTBMChan c >> writeBuffer c v
                else writeTBMChan c v

    revertBuffer c v = do
        b <- isClosedTBMChan c
        unless b $ do
            n <- estimateFreeSlotsTBMChan c
            unless (n <= 0) $ unGetTBMChan c v

    closeBuffer = closeTBMChan

newUnbounded :: STM (Unbounded a)
newUnbounded = newTMChan

newOverflow :: Int -> STM (Overflow a)
newOverflow = newTBMChan

sourceBuffer :: (MonadIO m, Buffer b) => b a -> Source m a
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

sinkBuffer :: (MonadIO m, Buffer b) => b a -> Sink a m ()
sinkBuffer buf =
      sink
    where
      sink       = NeedInput push (liftSTM $ closeBuffer buf)
      push input = PipeM (liftSTM $ writeBuffer buf input >> return sink)
                         (liftSTM $ closeBuffer buf)

liftSTM :: forall (m :: * -> *) a. MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
