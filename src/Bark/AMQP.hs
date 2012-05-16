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
import Data.Maybe                 (fromMaybe)
import Data.List.Split            (splitOn)
import Network.AMQP
import Network.URI                (URI(..), URIAuth(..), parseURI)

import qualified Data.ByteString    as BS
import qualified Data.HashTable.IO  as H
import qualified Bark.Message.Types as M

type Set k = H.BasicHashTable k Bool

data AMQPConn = AMQPConn
    { amqpLocal    :: BS.ByteString
    , amqpExchange :: BS.ByteString
    , amqpConn     :: Connection
    , amqpChan     :: Channel
    , amqpQueues   :: Set BS.ByteString
    , amqpBindings :: Set (BS.ByteString, BS.ByteString)
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

    queues   <- H.new
    bindings <- H.new

    return $ AMQPConn (pack hostname) (pack service) conn chan queues bindings
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

publish :: AMQPConn -> M.Message -> IO ()
publish conn@AMQPConn{..} msg = do
    (exchange, _) <- declare conn msg
    publishMsg amqpChan (unpack exchange) (unpack key) payload
  where
    key     = (M.publishKey msg amqpLocal)
    payload = newMsg { msgBody = fromChunks . body $ M.msgBody msg }

    body (M.Payload pay) = [pay]
    body (M.Error err)   = [err]

declare :: AMQPConn -> M.Message -> IO (BS.ByteString, BS.ByteString)
declare conn@AMQPConn{..} msg = do
    _ <- ensureQueue conn queue
    _ <- ensureBound conn queue key
    return (amqpExchange, queue)
  where
    queue = M.queue msg amqpExchange
    key   = M.bindKey msg

ensureQueue :: AMQPConn -> BS.ByteString -> IO ()
ensureQueue AMQPConn{..} queue = do
    exists <- H.lookup amqpQueues queue
    case exists of
        Nothing -> dec >> print inf >> ins
        Just _  -> return ()
  where
    dec = declareQueue amqpChan newQueue { queueName = unpack queue, queueDurable = True }
    ins = H.insert amqpQueues queue True
    inf = BS.concat ["Declare Q:", queue]

ensureBound :: AMQPConn -> BS.ByteString -> BS.ByteString -> IO ()
ensureBound AMQPConn{..} queue key = do
    exists <- H.lookup amqpBindings (queue, key)
    case exists of
        Nothing -> bind >> print inf >> ins
        Just _  -> return ()
  where
    bind = bindQueue amqpChan (unpack queue) (unpack amqpExchange) (unpack key)
    ins  = H.insert amqpBindings (queue, key) True
    inf  = BS.concat ["Binding Q:", queue, ", K:", key]

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '@')
