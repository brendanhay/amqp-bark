module Typhon.Reader (
      fill
    , drain
    , defaultBuffer
    , defaultWriter
    ) where

import System.IO (Handle, hGetLine)
import Typhon.Buffer
import Typhon.Writer

fill :: Buffer a => Handle -> a -> IO ()
fill fd buf = do
    line <- hGetLine fd
    buf' <- push buf line
    fd `fill` buf'

drain :: (Writer a, Buffer b) => a -> b -> IO ()
drain wrtr buf = do
    (buf', str) <- pop buf
    wrtr `write` str
    wrtr `drain` buf'