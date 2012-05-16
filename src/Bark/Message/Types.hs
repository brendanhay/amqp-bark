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

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

data Body = Payload B.ByteString | Error B.ByteString deriving (Eq, Show)

data Message = Message
    { msgCategory :: !B.ByteString
    , msgSeverity :: !B.ByteString
    , msgBody     :: !Body
    } deriving (Eq, Show)

defaultSeverity :: B.ByteString
defaultSeverity = "INFO"

defaultMessage :: Message
defaultMessage = Message
    { msgCategory = B.empty
    , msgSeverity = defaultSeverity
    , msgBody     = Payload B.empty
    }

queue :: Message -> B.ByteString -> B.ByteString
queue Message{..} app = normalise [app, msgCategory, msgSeverity]

publishKey :: Message -> B.ByteString -> B.ByteString
publishKey Message{..} prefix = normalise [prefix, msgCategory, msgSeverity]

bindKey :: Message -> B.ByteString
bindKey msg = publishKey msg "*"

--
-- Internal
--

normalise :: [B.ByteString] -> B.ByteString
normalise = C.map unboxedToLower . B.intercalate "."

unboxedToLower :: Char -> Char
unboxedToLower c@(C# c#)
    | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
    | isAscii c      = c
    | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
    | otherwise      = c