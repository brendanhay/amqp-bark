{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bark.Message (
      Body(..)
    , Message(..)
    , defaultSeverity
    , defaultMessage
    , queue
    , publishKey
    , bindKey
    ) where

import Data.ByteString.Char8 (pack, unpack)
import Data.Char             (toLower)
import Data.List             (intercalate)

import qualified Data.ByteString  as BS

data Body = Payload BS.ByteString | Error BS.ByteString deriving (Eq, Show)

data Message = Message
    { msgCategory :: !BS.ByteString
    , msgSeverity :: !BS.ByteString
    , msgBody     :: !Body
    } deriving (Eq, Show)

defaultSeverity :: BS.ByteString
defaultSeverity = "INFO"

defaultMessage :: Message
defaultMessage = Message
    { msgCategory = BS.empty
    , msgSeverity = defaultSeverity
    , msgBody     = Payload BS.empty
    }

queue :: Message -> String -> String
queue Message{..} app = safe [pack app, msgCategory, msgSeverity]

publishKey :: Message -> String -> String
publishKey Message{..} prefix = safe [pack prefix, msgCategory, msgSeverity]

bindKey :: Message -> String
bindKey msg = publishKey msg "*"

--
-- Internal
--

safe :: [BS.ByteString] -> String
safe = map toLower . intercalate "." . map unpack