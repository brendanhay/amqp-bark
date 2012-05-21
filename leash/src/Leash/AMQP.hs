{-# LANGUAGE RecordWildCards #-}

module Leash.AMQP
    ( subscribe

    -- * Network.AMQP re-exports
    , AMQPException
    , Connection
    , Message(..)
    , Envelope
    , closeConnection
    ) where

import Network.AMQP
import Bark.Types

import qualified Data.ByteString.Char8 as B

subscribe :: URI
          -> Host
          -> Service
          -> Category
          -> Severity
          -> ((Message, Envelope) -> IO ())
          -> IO Connection
subscribe URI{..} host serv cat sev callback = do
    conn <- openConnection uriHost uriVHost uriUser uriPass
    chan <- openChannel conn
    _    <- declareQueue chan opts
    _    <- bindQueue chan queue exchange key
    _    <- consumeMsgs chan queue NoAck callback
    return conn
  where
    queue    = ""
    exchange = B.unpack serv
    key      = publishKey host cat sev
    opts     = newQueue { queueName       = queue
                        , queueAutoDelete = True
                        , queueDurable    = False }
