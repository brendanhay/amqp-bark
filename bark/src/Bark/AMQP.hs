{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts,
    RankNTypes #-}

module Bark.AMQP (
    -- * Types
      PublishFailure(..)

    -- * Conduits
    , sinkAMQP

    -- * AMQP Connection
    , connect
    , disconnect
    ) where

import Control.Concurrent         (threadDelay)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Conduit
import Data.Hashable
import Network.AMQP
import Bark.Types
import Bark.IO

import qualified Data.ByteString.Char8 as C
import qualified Data.HashTable.IO     as H

type HashTable k v = H.BasicHashTable k v

data CacheKey = CacheKey Category Severity deriving (Eq)

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

data PublishFailure = PublishFailure Event deriving (Show, Ord, Eq)

sinkAMQP :: MonadIO m
         => AMQPConn
         -> Sink Event m (Maybe PublishFailure)
sinkAMQP conn =
    NeedInput push close
  where
    push evt = PipeM (attempt >>= either failure success) (return Nothing)
      where
        attempt   = liftTry $ publish conn evt
        failure _ = return $ Done Nothing (Just (PublishFailure evt))
        success _ = return $ NeedInput push close

    close = return Nothing

--
-- Internal
--

exchange :: Service -> ExchangeOpts
exchange serv = newExchange
    { exchangeName    = C.unpack serv
    , exchangeType    = "topic"
    , exchangeDurable = True
    }

queue :: String -> QueueOpts
queue name = newQueue
    { queueName    = name
    , queueDurable = True
    }

connect :: URI -> Int -> Host -> Service -> IO AMQPConn
connect URI{..} delay host serv =
    run
  where
    run = attempt >>= either failure return
      where
        attempt = liftTry $ do
            conn  <- openConnection uriHost uriVHost uriUser uriPass
            chan  <- openChannel conn
            cache <- H.new
            _     <- declareExchange chan $ exchange serv
            return $ AMQPConn host serv conn chan cache

        failure _ = liftIO $ threadDelay delay >> run

disconnect :: AMQPConn -> IO ()
disconnect = closeConnection . amqpConn

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
    _ <- declareQueue amqpChan $ queue bndQueue
    _ <- bindQueue amqpChan bndQueue bndExchange bndWildCard
    H.insert amqpCache idx bnd
    return bnd
  where
    bnd@Binding{..} = fromEvent evt amqpHost amqpService

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
