{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Leash.AMQP
import Leash.Options
import Bark.Types

import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts
    conn <- subscribe (parseURI optUri) optService optHost optCategory optSeverity recv
    putStrLn "Press any key to interrupt"
    _    <- getLine
    closeConnection conn
    putStrLn "done"

--
-- Internal
--

recv :: (Message, Envelope) -> IO ()
recv (msg, _) = putStrLn $ "line: " ++ L.unpack (msgBody msg)
