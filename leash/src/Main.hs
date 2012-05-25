{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Leash.AMQP
import Leash.Options

import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts

    putStrLn "Opening.."
    conn <- subscribe optUri optHost optService optCategory optSeverity recv

    putStrLn "Press any key to interrupt"
    _    <- getLine

    putStrLn "Closing.."
    closeConnection conn

--
-- Internal
--

recv :: (Message, Envelope) -> IO ()
recv (msg, _) = putStrLn $ L.unpack (msgBody msg)
