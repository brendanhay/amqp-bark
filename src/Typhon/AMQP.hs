{-# LANGUAGE OverloadedStrings #-}

module Typhon.AMQP (
      connection
    , channel
    , declare
    , publish
    ) where

import Data.Maybe      (fromMaybe)
import Data.List.Split (splitOn)
import Data.ByteString.Lazy.Char8 (pack)
import Text.URI        (URI(..), parseURI)
import Network.AMQP

-- API

connection :: URI -> IO Connection
connection uri = do
    let auth  = fromMaybe "guest:guest" $ uriUserInfo uri
    let host  = fromMaybe "127.0.0.1"   $ uriRegName uri
    let vhost = uriPath uri
    let [user, password] = splitOn ":" auth
    openConnection host vhost user password

channel :: Connection -> String -> String -> String -> IO Channel
channel conn exchange queue key = do
    chan <- openChannel conn
    declare chan exchange queue key
    return chan

publish :: Channel -> String -> String -> String -> IO ()
publish chan exchange queue payload =
    publishMsg chan exchange queue newMsg { msgBody = pack payload }

-- Private

declare :: Channel -> String -> String -> String -> IO ()
declare chan exchange queue key = do
    declareQueue chan newQueue { queueName = queue, queueDurable = True }
    declareExchange chan newExchange { exchangeName = exchange, exchangeType = "direct", exchangeDurable = True }
    bindQueue chan exchange queue key
