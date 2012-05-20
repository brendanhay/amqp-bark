{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bark.AMQP
    ( conduitAMQP
    , sinkAMQP
    ) where

import Control.Monad              (void)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Char8      (unpack)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Conduit
import Data.Hashable
import Network.AMQP        hiding (Message)
import Bark.Types

import qualified Data.ByteString   as B
import qualified Data.HashTable.IO as H
import qualified Network.AMQP      as A

conduitAMQP :: MonadResource m
            => URI
            -> Host
            -> Service
            -> Conduit Message m Message
conduitAMQP uri host serv =
    conduitIO (connect uri host serv) disconnect push close
  where
    push conn msg = do
        liftIO $ publish conn msg
        return $ IOProducing [msg]
    close conn    = liftIO $ disconnect conn >> return []

sinkAMQP :: MonadResource m
         => URI
         -> Host
         -> Service
         -> Sink Message m ()
sinkAMQP uri host serv =
    sinkIO (connect uri host serv) disconnect push close
  where
    push conn msg = liftIO (publish conn msg >> return IOProcessing)
    close conn    = liftIO . void $ disconnect conn

--
-- Internal
--

type HashTable k v = H.BasicHashTable k v

data CacheKey = CacheKey B.ByteString B.ByteString deriving (Eq)

instance Hashable CacheKey where
    hash (CacheKey cat sev) = hash [cat, sev]

type BindingCache = HashTable CacheKey Binding

data AMQPConn = AMQPConn
    { amqpHost    :: Host
    , amqpService :: Service
    , amqpConn    :: Connection
    , amqpChan    :: Channel
    , amqpCache   :: BindingCache
    }

disconnect :: AMQPConn -> IO ()
disconnect = closeConnection . amqpConn

connect :: URI -> Host -> Service -> IO AMQPConn
connect URI{..} host serv = do
    conn  <- openConnection uriHost uriVHost uriUser uriPass
    chan  <- openChannel conn
    cache <- H.new
    _     <- declareExchange chan opts
    return $ AMQPConn host serv conn chan cache
  where
    opts = newExchange { exchangeName = unpack serv
                       , exchangeType = "topic"
                       , exchangeDurable = True }

declare :: AMQPConn -> Message -> IO Binding
declare conn@AMQPConn{..} msg@Message{..} = do
    exists <- H.lookup amqpCache idx
    case exists of
        Just b  -> return b
        Nothing -> bind conn msg idx
  where
    idx = CacheKey messageCategory messageSeverity

bind :: AMQPConn -> Message -> CacheKey -> IO Binding
bind AMQPConn{..} msg idx = do
    _ <- declareQueue amqpChan opts
    _ <- bindQueue amqpChan boundQueue boundExchange boundDeclareKey
    H.insert amqpCache idx res
    return res
  where
    res@Binding{..} = fromMessage amqpService amqpHost msg
    opts            = newQueue { queueName = boundQueue, queueDurable = True }

publish :: AMQPConn -> Message -> IO ()
publish conn@AMQPConn{..} msg = do
    Binding{..} <- declare conn msg
    publishMsg amqpChan boundExchange boundPublishKey $ payload msg

payload :: Message -> A.Message
payload Message{..} =
    newMsg { msgBody = fromChunks $ chunk messageBody }
  where
    chunk (Payload pay) = [pay]
    chunk (Error err)   = [err]
