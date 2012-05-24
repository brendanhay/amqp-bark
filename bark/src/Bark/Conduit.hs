module Bark.Conduit
    ( sourceHandle
    , conduitHandle
    , conduitShow
    -- , conduitPublish
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import System.IO              (Handle, stdout)

import qualified Data.ByteString as B

sourceHandle :: MonadResource m
             => Handle
             -> Int
             -> Source m B.ByteString
sourceHandle handle limit =
    source
  where
    source = PipeM pull close
    pull = do
        bstr <- liftIO (B.hGetSome handle limit)
        if B.null bstr
            then return $ Done Nothing ()
            else return $ HaveOutput source close bstr
    close = return ()

conduitHandle :: MonadResource m
              => Handle
              -> Conduit B.ByteString m B.ByteString
conduitHandle handle =
    conduitIO (return handle) (\_ -> return ()) push (const $ return [])
  where
    push h input = do
        liftIO $ B.hPut h input
        return $ IOProducing [input]

conduitShow :: (Show a, MonadResource m) => Conduit a m a
conduitShow =
    conduitIO (return stdout) (\_ -> return ()) push (const $ return [])
  where
    push _ input = do
        liftIO $ print input
        return $ IOProducing [input]
