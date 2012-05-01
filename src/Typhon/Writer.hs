module Typhon.Writer (
      Writer(..)
    , defaultWriter
    ) where

import Typhon.AMQP

class Writer a where
    write :: a -> String -> IO ()

data AMQPWriter = AMQPWriter AMQPChan

instance Writer AMQPWriter where
    write _ []                  = return ()
    write (AMQPWriter chan) str = do
        putStrLn $ "publishing: " ++ str
        publish chan "typhon.ex" "typhon.q" str

defaultWriter :: IO AMQPWriter
defaultWriter = do
    chan <- connect "typhon.ex" "typhon.q"
    return $ AMQPWriter chan
