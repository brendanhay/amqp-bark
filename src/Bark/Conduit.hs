module Bark.Conduit (
      sourceHandle
    , conduitHandle
    , conduitShow
    ) where

import Control.Monad.IO.Class   (liftIO)
import Data.Conduit
import System.IO                (Handle, stdout)

import qualified Data.ByteString as BS

sourceHandle :: MonadResource m
             => Handle
             -> Int
             -> Source m BS.ByteString
sourceHandle handle buffer =
    source
  where
    source = PipeM pull close
    pull = do
        bstr <- liftIO (BS.hGetSome handle buffer)
        if BS.null bstr
            then return $ Done Nothing ()
            else return $ HaveOutput source close bstr
    close = return ()

conduitHandle :: MonadResource m
              => Handle
              -> Conduit BS.ByteString m BS.ByteString
conduitHandle handle =
    conduitIO (return handle) (\_ -> return ()) push (const $ return [])
  where
    push h input = do
        liftIO $ BS.hPut h input
        return $ IOProducing [input]

conduitShow :: (Show a, MonadResource m) => Conduit a m a
conduitShow =
    conduitIO (return stdout) (\_ -> return ()) push (const $ return [])
  where
    push _ input = do
        liftIO $ print input
        return $ IOProducing [input]
