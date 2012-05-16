{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bark.AMQP (
      URI
    , sinkAMQP
    , parseURI
    ) where

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Char8      (pack, unpack)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Conduit
import Data.Hashable
import Data.Maybe                 (fromMaybe)
import Data.List.Split            (splitOn)
import Network.AMQP
import Network.URI                (URI(..), URIAuth(..), parseURI)

import qualified Data.ByteString    as BS
import qualified Data.HashTable.IO  as H
import qualified Bark.Message.Types as M

type HashTable k v = H.BasicHashTable k v

data BindingKey = BindingKey BS.ByteString BS.ByteString deriving (Eq)

instance Hashable BindingKey where
    hash (BindingKey c s) = hash [c, s]

data Binding = Binding
    { boundExchange :: String
    , boundQueue    :: String
    , boundKey      :: String
    }

type BindingCache = HashTable BindingKey Binding

data AMQPConn = AMQPConn
    { amqpLocal    :: String
    , amqpExchange :: String
    , amqpConn     :: Connection
    , amqpChan     :: Channel
    , amqpBindings :: BindingCache
    }

sinkAMQP :: MonadResource m => URI -> String -> String -> Sink M.Message m ()
sinkAMQP uri hostname service =
    sinkIO (connect uri hostname service) disconnect push close
  where
    push conn msg = liftIO $ publish conn msg >> return IOProcessing
    close conn    = liftIO $ disconnect conn >> return ()

--
-- Internal
--

disconnect :: AMQPConn -> IO ()
disconnect = closeConnection . amqpConn

connect :: URI -> String -> String -> IO AMQPConn
connect uri hostname service = do
    conn <- connection uri
    chan <- openChannel conn

    declareExchange chan exchange

    bindings <- H.new

    return $ AMQPConn hostname service conn chan bindings
  where
    exchange = newExchange { exchangeName = service, exchangeType = "topic", exchangeDurable = True }

connection :: URI -> IO Connection
connection uri = do
    openConnection host vhost user pwd
  where
    auth = URIAuth "guest:guest" "127.0.0.1" ""
    (URIAuth info host _) = fromMaybe auth $ uriAuthority uri
    vhost = uriPath uri
    [user, pwd] = splitOn ":" $ trim info

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '@')

publish :: AMQPConn -> M.Message -> IO ()
publish conn@AMQPConn{..} msg = do
    Binding{..} <- declare conn msg
    publishMsg amqpChan boundExchange boundKey $ payload msg

payload :: M.Message -> Message
payload M.Message{..} =
    newMsg { msgBody = fromChunks $ chunk msgBody }
  where
    chunk (M.Payload pay) = [pay]
    chunk (M.Error err)   = [err]

declare :: AMQPConn -> M.Message -> IO Binding
declare conn@AMQPConn{..} msg@M.Message{..} = do
    exists <- H.lookup amqpBindings idx
    case exists of
        Just b  -> return b
        Nothing -> bind conn msg idx
  where
    idx = BindingKey msgCategory msgSeverity

bind :: AMQPConn -> M.Message -> BindingKey -> IO Binding
bind AMQPConn{..} msg idx = do
    _ <- declareQueue amqpChan newQueue { queueName = queue, queueDurable = True }
    _ <- bindQueue amqpChan queue amqpExchange key
    H.insert amqpBindings idx res
    return res
  where
    queue = unpack $ M.queue msg $ pack amqpExchange
    key   = unpack $ M.bindKey msg
    res   = Binding amqpExchange queue key
