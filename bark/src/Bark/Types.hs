{-# LANGUAGE OverloadedStrings, MagicHash, RecordWildCards #-}

module Bark.Types
    ( Host
    , Service
    , Category
    , Severity
    , RoutingKey
    , Binding(..)
    , Body(..)
    , Message(..)
    , URI(..)
    , defaultSeverity
    , mkBinding
    , fromMessage
    , publishKey
    , declareKey
    , parseURI
    ) where

import Data.Char       (isAsciiUpper, isAscii, isUpper)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, fromMaybe)
import GHC.Base

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Network.URI           as U

type Host = B.ByteString

type Service = B.ByteString

type Category = B.ByteString

type Severity = B.ByteString

type Exchange = String

type Queue = String

type RoutingKey = String

data Binding = Binding
    { boundExchange   :: Exchange
    , boundQueue      :: Queue
    , boundPublishKey :: RoutingKey
    , boundDeclareKey :: RoutingKey
    } deriving (Eq, Show)

data Body = Payload B.ByteString | Error B.ByteString deriving (Eq, Show)

data Message = Message
    { messageCategory :: !B.ByteString
    , messageSeverity :: !B.ByteString
    , messageBody     :: !Body
    } deriving (Eq, Show)

data URI = URI
    { uriUser  :: String
    , uriPass  :: String
    , uriHost  :: String
    , uriVHost :: String
    } deriving (Show, Eq)

defaultSeverity :: B.ByteString
defaultSeverity = "INFO"

mkBinding :: Host
          -> Service
          -> Category
          -> Severity
          -> Binding
mkBinding host serv cat sev =
    Binding exchange queue publish declare
  where
    exchange = C.unpack serv
    queue    = normalise [serv, cat, sev]
    publish  = publishKey host cat sev
    declare  = declareKey cat sev

fromMessage :: Host
            -> Service
            -> Message
            -> Binding
fromMessage serv host Message{..} =
    mkBinding serv host messageCategory messageSeverity

publishKey :: Host -> Category -> Severity -> RoutingKey
publishKey host cat sev = normalise [host, cat, sev]

declareKey :: Category -> Severity -> RoutingKey
declareKey cat sev = normalise [B.empty, cat, sev]

parseURI :: String -> URI
parseURI = conv . fromJust . U.parseURI

--
-- Internal
--

normalise :: [B.ByteString] -> String
normalise =
    C.unpack . C.map lowercase . B.intercalate "." . map wildcard
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
trim = f . f
  where
    f = reverse . dropWhile (== '@')
