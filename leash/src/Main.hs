{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Leash.AMQP
import Leash.Options
import Bark.Types

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    opts <- parseOptions
    print opts

    putStrLn "Opening.."
    conn <- connect opts

    putStrLn "Press any key to interrupt"
    _    <- getLine

    putStrLn "Closing.."
    closeConnection conn

--
-- Internal
--

connect :: Options -> IO Connection
connect Options{..} =
    subscribe uri host serv cat sev recv
  where
    uri  = parseURI optUri
    host = B.pack optHost
    serv = B.pack optService
    cat  = B.pack optCategory
    sev  = B.pack optSeverity

recv :: (Message, Envelope) -> IO ()
recv (msg, _) = putStrLn $ "line: " ++ L.unpack (msgBody msg)
