{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Data.Maybe          (fromJust)
import Network.URI         (parseURI)
import Leash.AMQP
import Leash.Options

import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts
    conn <- subscribe (fromJust $ parseURI optUri) optService optHost optCategory optSeverity recv
    _    <- getLine
    closeConnection conn
    putStrLn "done"

--
-- Internal
--

recv :: (Message, Envelope) -> IO ()
recv (msg, _) = putStrLn $ "line: " ++ L.unpack (msgBody msg)
