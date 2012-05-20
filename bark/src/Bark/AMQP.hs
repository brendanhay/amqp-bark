{-# LANGUAGE RecordWildCards #-}

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
import Network.AMQP
import Bark.Types

import qualified Data.ByteString   as B
import qualified Data.HashTable.IO as H

conduitAMQP :: MonadResource m
            => URI
            -> Host
            -> Service
            -> Conduit Event m Event
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
         -> Sink Event m ()
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

declare :: AMQPConn -> Event -> IO Binding
declare conn@AMQPConn{..} msg@Event{..} = do
    exists <- H.lookup amqpCache idx
    case exists of
        Just b  -> return b
        Nothing -> bind conn msg idx
  where
    idx = CacheKey evtCategory evtSeverity

bind :: AMQPConn -> Event -> CacheKey -> IO Binding
bind AMQPConn{..} evt idx = do
    _ <- declareQueue amqpChan opts
    _ <- bindQueue amqpChan bndQueue bndExchange bndWildCard
    H.insert amqpCache idx bnd
    return bnd
  where
    bnd@Binding{..} = fromEvent evt amqpHost amqpService
    opts            = newQueue { queueName = bndQueue, queueDurable = True }

publish :: AMQPConn -> Event -> IO ()
publish conn@AMQPConn{..} msg = do
    Binding{..} <- declare conn msg
    publishMsg amqpChan bndExchange bndExplicit $ payload msg

payload :: Event -> Message
payload Event{..} =
    newMsg { msgBody = fromChunks $ chunk evtBody }
  where
    chunk (Payload pay) = [pay]
    chunk (Error err)   = [err]
