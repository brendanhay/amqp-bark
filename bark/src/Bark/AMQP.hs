{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, RankNTypes #-}

module Bark.AMQP
    ( sinkAMQP

    -- * Network.AMQP re-exports
    , AMQPException(..)
    , ConnectionException(..)
    ) where

import Data.Data (Typeable)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted   (try, throwIO)
import Control.Monad              (void)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Conduit
import Data.Hashable
import Network.AMQP
import Bark.Types
import Control.Monad.Trans.Control (MonadBaseControl)
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

data ConnectionException = ConnectionException Event deriving (Typeable, Show, Ord, Eq)

instance Exception ConnectionException

sinkAMQP :: (MonadBaseControl IO m, MonadResource m)
         => URI
         -> Host
         -> Service
         -> Sink Event m ()
sinkAMQP uri host serv =
    sinkIO (alloc 60000) disconnect push close
  where
    alloc n = liftIO $ do
        ex <- try' $ connect uri host serv
        case ex of
            Right conn -> return conn
            Left _ -> do
                putStrLn $ "Retrying in " ++ show n
                threadDelay n
                alloc (n + 10000)

    push conn evt = liftIO $ do
        ex <- try' $ publish conn evt
        case ex of
            Right _ -> return IOProcessing
            Left _ -> do
                -- resource dealloc needs to occur here via release
                disconnect conn
                throwIO $ ConnectionException evt

    close conn = liftIO . void $ disconnect conn

    try' :: MonadBaseControl IO m => m a -> m (Either SomeException a)
    try' = try

--
-- Internal
--

connect :: URI -> Host -> Service -> IO AMQPConn
connect URI{..} host serv = do
    conn  <- openConnection uriHost uriVHost uriUser uriPass
    chan  <- openChannel conn
    cache <- H.new
    _     <- declareExchange chan opts
    return $ AMQPConn host serv conn chan cache
  where
    opts = newExchange { exchangeName    = C.unpack serv
                       , exchangeType    = "topic"
                       , exchangeDurable = True }

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
    _ <- declareQueue amqpChan opts
    _ <- bindQueue amqpChan bndQueue bndExchange bndWildCard
    H.insert amqpCache idx bnd
    return bnd
  where
    bnd@Binding{..} = fromEvent evt amqpHost amqpService
    opts = newQueue { queueName    = bndQueue
                    , queueDurable = True }

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
