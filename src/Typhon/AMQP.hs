{-# LANGUAGE OverloadedStrings #-}

module Typhon.AMQP (
      connect
    , publish
    , AMQPConn
    , AMQPChan
    , URI
    , parseURI
    ) where

import Data.Maybe      (fromJust, fromMaybe)
import Data.List.Split (splitOn)
import Text.URI        (URI(..), parseURI)
import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL

type AMQPConn = Connection
type AMQPChan = Channel

-- API

connect :: String -> String -> IO AMQPChan
connect exchange queue = do
    let amqpURI = "amqp://guest:guest@127.0.0.1/"
    let uri   = fromJust $ parseURI amqpURI
    let auth  = fromMaybe "guest:guest" $ uriUserInfo uri
    let host  = fromMaybe "127.0.0.1"   $ uriRegName uri
    let vhost = uriPath uri

    let [user, password] = splitOn ":" auth

    conn <- openConnection host vhost user password
    chan <- openChannel conn

--    declare chan exchange queue queue
    putStrLn ("Queue: " ++ queue)
    declareQueue chan newQueue { queueName = queue, queueDurable = True }
    putStrLn ("Ex: " ++ exchange)
    declareExchange chan newExchange { exchangeName = exchange, exchangeType = "fanout", exchangeDurable = True }
    bindQueue chan queue exchange queue

    return chan

publish :: AMQPChan -> String -> String -> String -> IO ()
publish chan exchange queue payload =
    publishMsg chan exchange queue newMsg { msgBody = BL.pack payload }

-- Private

declare :: AMQPChan -> String -> String -> String -> IO ()
declare chan exchange queue key = do
    declareQueue chan newQueue { queueName = queue, queueDurable = True }
    declareExchange chan newExchange { exchangeName = exchange, exchangeType = "fanout", exchangeDurable = True }
    bindQueue chan queue exchange key

