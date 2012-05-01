module Typhon.Buffer (
      Buffer(..)
    , defaultBuffer
    ) where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.List.Split         (splitOn)

class Buffer a where
    push :: a -> String -> IO a
    pop  :: a -> IO (a, String)

data AsyncBuffer = AsyncBuffer
    { bufChannel   :: Chan String
    , bufContents  :: String
    , bufDelimiter :: String
    }

instance Buffer AsyncBuffer where
    push buf@(AsyncBuffer chan _ _) line = do
        putStrLn $ "writing to buf: " ++ line
        writeChan chan line
        return buf
    pop buf@(AsyncBuffer chan str del) = do
        line <- readChan chan
        -- check if line ends with delimiter and just return
        let (overflow, contents) = split del (str ++ line)
        putStrLn $ "buf contents: " ++ contents
        return (buf { bufContents = contents }, overflow)

defaultBuffer :: IO AsyncBuffer
defaultBuffer = do
    chan <- newChan
    return $ AsyncBuffer chan [] "--"

split :: String -> String -> (String, String)
split del str = case splitOn del str of
    x:[] -> ([], x)
    xs   -> (head xs, concat $ tail xs)