{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bark.Message (
      Body(..)
    , Message(..)
    , defaultSeverity
    , defaultMessage
    , exchange
    , queue
    , publishKey
    , bindKey
    ) where

import Data.ByteString.Char8 (pack, unpack)
import Data.List             (intercalate)

import qualified Data.ByteString  as BS

data Body = Payload BS.ByteString | Error BS.ByteString deriving (Eq, Show)

data Message = Message
    { msgApp      :: !String
    , msgCategory :: !BS.ByteString
    , msgSeverity :: !BS.ByteString
    , msgBody     :: !Body
    } deriving (Eq, Show)

defaultSeverity :: BS.ByteString
defaultSeverity = "INFO"

defaultMessage = Message
    { msgApp      = ""
    , msgCategory = BS.empty
    , msgSeverity = defaultSeverity
    , msgBody     = Payload BS.empty
    }

exchange :: Message -> String
exchange Message{..} = msgApp

queue :: Message -> String
queue Message{..} = safe [pack msgApp, msgCategory, msgSeverity]

publishKey :: Message -> String -> String
publishKey Message{..} prefix = safe [pack prefix, msgCategory, msgSeverity]

bindKey :: Message -> String
bindKey msg = publishKey msg "*"

--
-- Internal
--

safe :: [BS.ByteString] -> String
safe = intercalate "." . map unpack