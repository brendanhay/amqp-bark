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
          -> ((Message,Envelope) -> IO ())
          -> IO Connection
subscribe uri service host cat sev callback = do
    conn  <- mkConnection uri
    chan  <- openChannel conn
    _     <- declareQueue chan newQueue { queueName = queue, queueAutoDelete = True, queueDurable = False }
    _     <- bindQueue chan queue exchange key
    _     <- consumeMsgs chan queue NoAck callback
    return $! conn
  where
    queue    = ""
    exchange = service
    key      = routingKey host cat sev

--
-- Internal
--

mkConnection :: URI -> IO Connection
mkConnection uri =
    openConnection host vhost user pwd
  where
    auth = URIAuth "guest:guest" "127.0.0.1" ""
    (URIAuth info host _) = fromMaybe auth $ uriAuthority uri
    vhost = uriPath uri
    [user, pwd] = splitOn ":" $ trim info

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '@')
