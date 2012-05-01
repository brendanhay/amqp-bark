module Typhon.Reader (
      buffer
    , flush
    , defaultBuffer
    , defaultWriter
    ) where

import System.IO (Handle, hGetLine)
import Typhon.Buffer
import Typhon.Writer

buffer :: Buffer a => Handle -> a -> IO ()
buffer fd buf = do
    line <- hGetLine fd
    buf' <- push buf line
    buffer fd buf'

flush :: (Writer a, Buffer b) => a -> b -> IO ()
flush wtr buf = do
    (buf', str) <- pop buf
    write wtr str
    flush wtr buf'