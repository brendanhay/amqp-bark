{-# LANGUAGE RecordWildCards #-}

module Bark.AMQP (
      URI
    , sinkAMQP
    , parseURI
    ) where

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Conduit
import Data.Maybe                 (fromMaybe)
import Data.List.Split            (splitOn)
import Network.AMQP
import Network.BSD                (getHostName)
import Network.URI                (URI(..), URIAuth(..), parseURI)

import qualified Data.HashTable.IO  as H
import qualified Bark.Message.Types as M

type HashTable k v = H.BasicHashTable k v

data AMQPConn = AMQPConn
    { amqpExchange :: String
    , amqpConn     :: Connection
    , amqpChan     :: Channel
    , amqpQueues   :: HashTable String Bool
    , amqpBindings :: HashTable (String, String) Bool
    }

sinkAMQP :: MonadResource m => URI -> String -> Sink M.Message m ()
sinkAMQP uri app =
    sinkIO (connect uri app) disconnect push close
  where
    push conn msg = liftIO $ publish conn msg >> return IOProcessing
    close conn    = liftIO $ disconnect conn >> return ()

--
-- Internal
--

disconnect :: AMQPConn -> IO ()
disconnect = closeConnection . amqpConn

connect :: URI -> String -> IO AMQPConn
connect uri app = do
    conn <- connection uri
    chan <- openChannel conn

    declareExchange chan exchange

    queues   <- H.new
    bindings <- H.new

    return $ AMQPConn app conn chan queues bindings
  where
    exchange = newExchange { exchangeName = app, exchangeType = "topic", exchangeDurable = True }

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
    host          <- hostName
    publishMsg amqpChan exchange (M.publishKey msg host) payload
  where
    payload            = newMsg { msgBody = fromChunks . body $ M.msgBody msg }
    body (M.Payload b) = [b]
    body (M.Error err) = [err]

declare :: AMQPConn -> M.Message -> IO (String, String)
declare conn@AMQPConn{..} msg = do
    _ <- ensureQueue conn queue
    _ <- ensureBound conn queue key
    return (amqpExchange, queue)
  where
    queue    = M.queue msg amqpExchange
    key      = M.bindKey msg

ensureQueue :: AMQPConn -> String -> IO ()
ensureQueue AMQPConn{..} queue = do
    exists <- H.lookup amqpQueues queue
    case exists of
        Nothing -> dec >> putStrLn ("Declare Q:" ++ queue) >> ins
        Just _  -> return ()
  where
    dec = declareQueue amqpChan newQueue { queueName = queue, queueDurable = True }
    ins = H.insert amqpQueues queue True

ensureBound :: AMQPConn -> String -> String -> IO ()
ensureBound AMQPConn{..} queue key = do
    exists <- H.lookup amqpBindings (queue, key)
    case exists of
        Nothing -> bind >> putStrLn inf >> ins
        Just _  -> return ()
  where
    bind = bindQueue amqpChan queue amqpExchange key
    ins  = H.insert amqpBindings (queue, key) True
    inf  = concat ["Binding Q:", queue, ", K:", key]

hostName :: IO String
hostName = getHostName >>= return . map f
  where
    f '.' = '_'
    f c   = c

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '@')
