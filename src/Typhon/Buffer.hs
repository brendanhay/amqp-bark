module Typhon.Buffer (
      Buffer(..)
    , defaultBuffer
    ) where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

class Buffer a where
    push :: a -> String -> IO ()
    pop  :: a -> IO (a, String)

data AsyncBuffer = AsyncBuffer (Chan String) String

instance Buffer AsyncBuffer where
    push (AsyncBuffer chan _) line = do
        putStrLn $ "writing to buf: " ++ line
        writeChan chan line
    pop buf@(AsyncBuffer chan _) = do
        line <- readChan chan
        putStrLn $ "popping from buf: " ++ line
        return (buf, line)

defaultBuffer :: IO AsyncBuffer
defaultBuffer = do
    chan <- newChan
    return $ AsyncBuffer chan []