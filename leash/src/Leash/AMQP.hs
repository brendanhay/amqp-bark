{-# LANGUAGE RecordWildCards #-}

module Leash.AMQP
    ( subscribe

    -- * Netork.AMQP re-exports
    , Connection
    , Message(..)
    , Envelope
    , closeConnection
    ) where

import Network.AMQP
import Bark.Types

subscribe :: URI
          -> String
          -> Host
          -> Category
          -> Severity
          -> ((Message, Envelope) -> IO ())
          -> IO Connection
subscribe URI{..} service host cat sev callback = do
    conn  <- openConnection uriHost uriVHost uriUser uriPass
    chan  <- openChannel conn
    _     <- declareQueue chan newQueue { queueName = queue, queueAutoDelete = True, queueDurable = False }
    _     <- bindQueue chan queue exchange key
    _     <- consumeMsgs chan queue NoAck callback
    return $! conn
  where
    queue    = ""
    exchange = service
    key      = publishKey host cat sev
