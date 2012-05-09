module Network.AMQP.Conduit (
    -- * Sources, Sinks, and Conduits
      conduitAMQP
    , sinkAMQP

    -- * Text.URI re-exports
    , URI
    , parseURI

    -- * Network.AMQP re-exports
    , ExchangeOpts(..)
    , QueueOpts(..)
    , newExchange
    , newQueue
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit
import Data.Maybe             (fromJust, fromMaybe)
import Data.List.Split        (splitOn)
import Network.URI            (URI(..), URIAuth(..), parseURI)
import Network.AMQP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data AMQPConn = AMQPConn
    { amqpConn     :: Connection
    , amqpChan     :: Channel
    , amqpQueue    :: ExchangeOpts
    , amqpExchange :: QueueOpts
    }

conduitAMQP :: MonadResource m
            => URI
            -> ExchangeOpts
            -> QueueOpts
            -> Conduit BS.ByteString m BS.ByteString
conduitAMQP uri exchange queue =
    conduitIO
    (connect uri exchange queue)
    disconnect
    (\conn bstr -> push (IOProducing [bstr]) conn bstr)
    (close [])

sinkAMQP :: MonadResource m
         => URI
         -> ExchangeOpts
         -> QueueOpts
         -> Sink BS.ByteString m ()
sinkAMQP uri exchange queue =
    sinkIO
    (connect uri exchange queue)
    disconnect
    (push IOProcessing)
    (close ())

--
-- Conduit Helpers
--

push :: MonadResource m => b -> AMQPConn -> BS.ByteString -> m b
push res conn bstr = do
    liftIO $ publish conn bstr
    return res

close :: MonadResource m => a -> AMQPConn -> m a
close res conn = do
    liftIO $ disconnect conn
    return res

--
-- Internal
--

publish :: AMQPConn -> BS.ByteString -> IO ()
publish (AMQPConn _ chan exchange queue) payload =
    publishMsg chan (exchangeName exchange) (queueName queue) message
  where
    message = newMsg { msgBody = BL.fromChunks [payload] }

connect :: URI -> ExchangeOpts -> QueueOpts -> IO AMQPConn
connect uri exchange queue = do
    conn <- openConn uri
    chan <- openChannel conn

    declareQueue chan queue
    declareExchange chan exchange

    bindQueue chan (exchangeName exchange) key key

    return $ AMQPConn conn chan exchange queue
  where
    key = queueName queue

disconnect :: AMQPConn -> IO ()
disconnect = closeConnection . amqpConn

openConn :: URI -> IO Connection
openConn uri = do
    openConnection host vhost user pwd
  where
    auth = URIAuth "guest:guest" "127.0.0.1" ""
    (URIAuth info host _) = fromMaybe auth $ uriAuthority uri
    vhost = uriPath uri
    [user, pwd] = splitOn ":" $ trim info

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== '@')