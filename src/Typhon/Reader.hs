module Typhon.Reader (
      drain
    , flush
    , defaultBuffer
    , defaultWriter
    ) where

import System.IO (Handle, hGetLine)
import Typhon.Buffer
import Typhon.Writer

drain :: Buffer a => Handle -> a -> IO ()
drain fd buf = do
    line <- hGetLine fd
    buf' <- push buf line
    drain fd buf'

flush :: (Writer a, Buf b) => a -> b -> IO ()
flush wrt buf = do
    (buf', str) <- pop buf
    write wrt str
    flush wrt buf'