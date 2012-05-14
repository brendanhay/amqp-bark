{-# LANGUAGE RecordWildCards #-}

module Bark.AMQP (
    -- * Sources, Sinks, and Conduits
      sinkAMQP

    -- * Text.URI re-exports
    , URI
    , parseURI
    ) where

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Char                  (toLower)
import Data.Conduit
import Data.Maybe                 (fromJust, fromMaybe, isJust)
import Data.List.Split            (splitOn)
import Network.AMQP
import Network.BSD                (getHostName)
import Network.URI                (URI(..), URIAuth(..), parseURI)

import qualified Data.ByteString   as BS
import qualified Data.HashTable.IO as H
import qualified Bark.Message      as M

type HashTable k v = H.BasicHashTable k v

data AMQPConn = AMQPConn
    { amqpConn     :: Connection
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
    conn   <- connection uri
    chan   <- openChannel conn
    cache1 <- H.new
    cache2 <- H.new

    declareExchange chan newExchange { exchangeName = app, exchangeType = "topic", exchangeDurable = True }

    return $ AMQPConn conn chan cache1 cache2

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
    putStrLn $ "Publish: " ++ (M.publishKey msg host)
    publishMsg amqpChan exchange (M.publishKey msg host) payload
  where
    payload            = newMsg { msgBody = body $ M.msgBody msg }
    body (M.Payload b) = fromChunks [b]
    body (M.Error err) = fromChunks [err]

declare :: AMQPConn -> M.Message -> IO (String, String)
declare conn msg = do
    _ <- ensureQueue conn queue
    _ <- ensureBound conn exchange queue key
    return (exchange, queue)
  where
    exchange = M.exchange msg
    queue    = M.queue msg
    key      = M.bindKey msg

ensureQueue :: AMQPConn -> String -> IO ()
ensureQueue AMQPConn{..} queue = do
    exists <- H.lookup amqpQueues queue
    case exists of
        Nothing -> dec >> putStrLn ("Declare: " ++ queue) >> ins
        Just _  -> return ()
  where
    dec = declareQueue amqpChan newQueue { queueName = queue, queueDurable = True }
    ins = H.insert amqpQueues queue True

ensureBound :: AMQPConn -> String -> String -> String -> IO ()
ensureBound AMQPConn{..} exchange queue key = do
    exists <- H.lookup amqpBindings (queue, key)
    case exists of
        Nothing -> bind >> putStrLn ("Bind: " ++ key) >> ins
        Just _  -> return ()
  where
    bind = bindQueue amqpChan queue exchange key
    ins  = H.insert amqpBindings (queue, key) True

hostName :: IO String
hostName = getHostName >>= return . map f
  where
    f '.' = '_'
    f c   = toLower c

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '@')
