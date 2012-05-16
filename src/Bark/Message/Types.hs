{-# LANGUAGE OverloadedStrings, RecordWildCards, MagicHash #-}

module Bark.Message.Types (
      Body(..)
    , Message(..)
    , defaultSeverity
    , defaultMessage
    , queue
    , publishKey
    , bindKey
    ) where

import GHC.Base
import Data.Char (isAsciiUpper, isAscii, isUpper)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C

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

queue :: Message -> BS.ByteString -> BS.ByteString
queue Message{..} app = normalise [app, msgCategory, msgSeverity]

publishKey :: Message -> BS.ByteString -> BS.ByteString
publishKey Message{..} prefix = normalise [prefix, msgCategory, msgSeverity]

bindKey :: Message -> BS.ByteString
bindKey msg = publishKey msg "*"

--
-- Internal
--

normalise :: [BS.ByteString] -> BS.ByteString
normalise = C.map unboxedToLower . BS.intercalate "."

unboxedToLower :: Char -> Char
unboxedToLower c@(C# c#)
    | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
    | isAscii c      = c
    | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
    | otherwise      = c