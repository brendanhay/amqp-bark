{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main (
      main
    ) where

import System.Console.CmdArgs
import Control.Applicative     ((<$>), (<*>))
import Control.Monad           (mzero)
import Control.Monad.Fix       (fix)
import Control.Concurrent      (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Maybe              (fromJust, fromMaybe)
import Text.URI                (URI(..), parseURI)

version :: String
version = "0.1"

data Options = Options
    { optName :: String
    , optType :: String
    } deriving (Show, Data, Typeable)

options = Options
     { optName = def &= explicit &= name "name" &= help "Application name" &= typ "NAME"
     , optType = def &= explicit &= name "type" &= help "Application type" &= typ "TYPE"
     , optUri  = def &= explicit &= name "uri"  &= help "AMQP Uri"         &= typ "URI"
     } &= summary ("Typhon " ++ version)

main :: IO ()
main = do
    opts <- parseArgs
    chan <- fork opts
    recv chan opts

parseArgs :: IO Options
parseArgs = do
    opts <- cmdArgs options
    return $ valid opts

valid :: Options -> Options
valid Options { optName = [] } = error "name cannot be blank"
valid Options { optType = [] } = error "type cannot be blank"
valid opts                     = opts

fork :: Options -> Chan String
fork opts = do
    chan <- newChan
    forkIO $ send chan opts
    chan

recv :: Chan String -> Options -> IO ()
recv chan opts = do
    line <- getLine
    putStrLn $ "Received: " ++ line
    writeChan chan line
    recv opts chan

send :: Chan String -> Options -> IO ()
send chan opts = do
    line <- readChan chan
    putStrLn ("Sending: " ++ line)
    send chan

amqpUri :: IO String
amqpUri = do
    uri <- getEnvDefault "AMQP_URI" "amqp://guest:guest@127.0.0.1/"
    return $ fromJust $ parseURI uri

amqpConn :: IO AMQPConn
amqpConn = do
    uri <- amqpUri

    let auth  = fromMaybe "guest:guest" $ uriUserInfo uri
    let host  = fromMaybe "127.0.0.1"   $ uriRegName uri
    let vhost = uriPath uri

    let [user, password] = split ":" auth

    openConnection host vhost user password

amqpChan :: IO AMQPConn
    conn <- amqpConn
    chan <- openChannel conn

    amqpBind chan "typhon.test.ex" "typhon.test.q" "Event"

    listener <- newChan
    forkIO $ fix $ \loop -> readChan listener >> loop
    return (chan, listener)

amqpListener = do

amqpBind chan exchange queue key = do
    amqpQueue chan queue
    amqpExchange chan exchange
    bindQueue chan exchange queue key

amqpQueue chan queue =
    declareQueue chan newQueue { queueName = queue, queueDurable = True }

amqpExchange chan exchange =
    declareExchange chan newExchange { exchangeName = exchange, exchangeType = "direct", exchangeDurable = True }