{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MagicHash,
    RecordWildCards #-}

module Bark.Types (
    -- * Exported Types
      Host(..)
    , Service(..)
    , Category(..)
    , Severity(..)
    , Binding(..)
    , Body(..)
    , Event(..)
    , URI(..)

    -- * Strings
    , ToString(..)

    -- * Defaults
    , defaultSeverity

    -- * Constructors
    , mkBinding
    , fromEvent
    , mkEvent
    , parseURI

    -- * Routing
    , publishKey
    , declareKey
    ) where

import Data.Char       (isAsciiUpper, isAscii, isUpper)
import Data.Data (Data, Typeable)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, fromMaybe)
import Data.String     (IsString(..))
import GHC.Base

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Network.URI           as U

newtype Host = Host B.ByteString deriving (Data, Typeable, Show, Eq)

newtype Service = Service B.ByteString deriving (Data, Typeable, Show, Eq)

newtype Category = Category B.ByteString deriving (Eq, Show)

newtype Severity = Severity B.ByteString deriving (Eq, Show)

data Binding = Binding
    { bndExchange :: String
    , bndQueue    :: String
    , bndExplicit :: String
    , bndWildCard :: String
    } deriving (Eq, Show)

data Body = Payload B.ByteString | Error B.ByteString deriving (Eq, Show)

data Event = Event
    { evtCategory :: !Category
    , evtSeverity :: !Severity
    , evtBody     :: !Body
    } deriving (Eq, Show)

data URI = URI
    { uriUser  :: String
    , uriPass  :: String
    , uriHost  :: String
    , uriVHost :: String
    } deriving (Show, Eq)

--
-- Strings
--

instance IsString Host where
    fromString s = Host $ C.pack s

instance IsString Service where
    fromString s = Service $ C.pack s

class ToString a where
    toString :: a -> String

instance ToString Service where
    toString (Service serv) = C.unpack serv

--
-- Defaults
--

defaultSeverity :: B.ByteString
defaultSeverity = "INFO"

--
-- Constructors
--

mkBinding :: Host
          -> Service
          -> Category
          -> Severity
          -> Binding
mkBinding host (Service serv) cat sev =
    Binding exchange queue publish declare
  where
    exchange = C.unpack serv
    queue    = name serv cat sev
    publish  = publishKey host cat sev
    declare  = declareKey cat sev

fromEvent :: Event -> Host -> Service -> Binding
fromEvent Event{..} host serv = mkBinding host serv evtCategory evtSeverity

mkEvent :: B.ByteString -> B.ByteString -> Body -> Event
mkEvent cat sev body = Event (Category cat) (Severity sev) body

parseURI :: String -> URI
parseURI = conv . fromJust . U.parseURI

--
-- Routing
--

publishKey :: Host -> Category -> Severity -> String
publishKey (Host h) = name h

declareKey :: Category -> Severity -> String
declareKey = name B.empty

--
-- Internal
--

name :: B.ByteString -> Category -> Severity -> String
name prefix (Category cat) (Severity sev) =
    C.unpack $ normalise [prefix, cat, sev]

normalise :: [B.ByteString] -> B.ByteString
normalise = C.map lowercase . B.intercalate "." . map wildcard
  where
    wildcard bstr | B.null bstr = "*"
                  | otherwise   = bstr

lowercase :: Char -> Char
lowercase c@(C# c#)
    | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
    | isAscii c      = c
    | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
    | otherwise      = c

conv :: U.URI -> URI
conv uri = URI host vhost user pass
  where
    auth = U.URIAuth "guest:guest" "127.0.0.1" ""
    (U.URIAuth info host _) = fromMaybe auth $ U.uriAuthority uri
    vhost = U.uriPath uri
    [user, pass] = splitOn ":" $ trim info

trim :: String -> String
trim =
    f . f
  where
    f = reverse . dropWhile (== '@')
