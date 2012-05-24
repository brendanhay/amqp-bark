{-# LANGUAGE FlexibleContexts, KindSignatures, RankNTypes #-}

module Bark.Exception (
      liftTry
    ) where

import Control.Exception           (SomeException)
import Control.Exception.Lifted    (try)
import Control.Monad.IO.Class      (MonadIO, liftIO)

liftTry :: MonadIO m => IO a -> m (Either SomeException a)
liftTry = liftIO . try
