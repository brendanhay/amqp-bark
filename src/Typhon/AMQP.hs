{-# LANGUAGE OverloadedStrings #-}

module Typhon.AMQP (
      newChan
    , publish
    , Network.AMQP.Connection
    , Network.AMQP.Channel
    , URI
    , parseURI
    ) where

import Data.Maybe      (fromJust, fromMaybe)
import Data.List.Split (splitOn)
import Text.URI        (URI(..), parseURI)
import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL

newChan :: String -> String -> IO Channel
newChan exchange queue = do
    let amqpURI = "amqp://guest:guest@127.0.0.1/"
    let uri   = fromJust $ parseURI amqpURI
    let auth  = fromMaybe "guest:guest" $ uriUserInfo uri
    let host  = fromMaybe "127.0.0.1"   $ uriRegName uri
    let vhost = uriPath uri

    let [user, password] = splitOn ":" auth

    conn <- openConnection host vhost user password
    chan <- openChannel conn

    declareQueue chan newQueue { queueName = queue, queueDurable = True }
    declareExchange chan newExchange { exchangeName = exchange, exchangeType = "fanout", exchangeDurable = True }
    bindQueue chan queue exchange queue

    return chan

publish :: Channel -> String -> String -> String -> IO ()
publish chan exchange queue payload =
    publishMsg chan exchange queue newMsg { msgBody = BL.pack payload }
