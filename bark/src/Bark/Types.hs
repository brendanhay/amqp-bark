{-# LANGUAGE OverloadedStrings, MagicHash, RecordWildCards #-}

module Bark.Types (
    -- * Exported Aliases
      Host
    , Service
    , Category
    , Severity

    -- * Exported Types
    , Binding(..)
    , Body(..)
    , Event(..)
    , URI(..)

    -- * Defaults
    , defaultCategory
    , defaultSeverity

    -- * Constructors
    , mkBinding
    , mkEvent
    , fromEvent
    , parseURI

    -- * Routing
    , publishKey
    , declareKey
    ) where

import Data.Char       (isAsciiUpper, isAscii, isUpper)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, fromMaybe)
import GHC.Base

import qualified Data.ByteString.Char8 as B
import qualified Network.URI           as U

type Host     = B.ByteString
type Service  = B.ByteString
type Category = B.ByteString
type Severity = B.ByteString

data Binding = Binding
    { bndExchange :: String
    , bndQueue    :: String
    , bndExplicit :: String
    , bndWildCard :: String
    } deriving (Eq, Show)

data Body = Payload B.ByteString | Error B.ByteString deriving (Ord, Eq, Show)

data Event = Event
    { evtRetries  :: !Int
    , evtCategory :: !Category
    , evtSeverity :: !Severity
    , evtBody     :: !Body
    } deriving (Ord, Eq, Show)

data URI = URI
    { uriUser  :: String
    , uriPass  :: String
    , uriHost  :: String
    , uriVHost :: String
    } deriving (Show, Eq)

--
-- Defaults
--

defaultCategory :: B.ByteString
defaultCategory = "EVENT"

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
mkBinding host serv cat sev =
    Binding exchange queue publish declare
  where
    exchange = B.unpack serv
    queue    = name serv cat sev
    publish  = publishKey host cat sev
    declare  = declareKey cat sev

mkEvent :: Category -> Severity -> Body -> Event
mkEvent = Event 0

fromEvent :: Event -> Host -> Service -> Binding
fromEvent Event{..} host serv = mkBinding host serv evtCategory evtSeverity

parseURI :: String -> URI
parseURI = conv . fromJust . U.parseURI

--
-- Routing
--

publishKey :: Host -> Category -> Severity -> String
publishKey = name

declareKey :: Category -> Severity -> String
declareKey = name B.empty

--
-- Internal
--

name :: B.ByteString -> Category -> Severity -> String
name prefix cat sev = B.unpack $ normalise [prefix, cat, sev]

normalise :: [B.ByteString] -> B.ByteString
normalise = B.map lowercase . B.intercalate "." . map wildcard
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
conv uri = URI user pass host vhost
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
