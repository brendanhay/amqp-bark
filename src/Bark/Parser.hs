{-# LANGUAGE OverloadedStrings #-}

module Bark.Parser (
      Message(..)
    , conduitMessage
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.Conduit
import Data.Monoid (mempty)
import GHC.Word    (Word8)

import qualified Data.ByteString       as BS
import qualified Data.Attoparsec.Char8 as AC

newtype Severity = Severity BS.ByteString deriving (Eq, Show)

newtype Category = Category BS.ByteString deriving (Eq, Show)

data Body = Payload BS.ByteString | Error String deriving (Eq, Show)

data Message = Message
    { msgSeverity :: !Severity
    , msgCategory :: !Category
    , msgBody     :: !Body
    } deriving (Eq, Show)

defaultMessage = Message
    { msgSeverity = Severity BS.empty
    , msgCategory = Category BS.empty
    , msgBody     = Payload BS.empty
    }

conduitMessage :: MonadResource m => Conduit BS.ByteString m Message
conduitMessage =
    conduit
  where
    conduit     = NeedInput push mempty
    push        = HaveOutput conduit (return ()) . parse
    parse input = case parseOnly messageParser' input of
        Right msg -> msg
        Left  err -> Message (Severity "ERROR") (Category "error") (Error err)

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'

bracketedValue :: Parser BS.ByteString
bracketedValue = bracket *> AC.takeTill (== ']') <* unbracket

messageParser' :: Parser Message
messageParser' = do
    severity <- bracketedValue
    category <- bracketedValue <|> pure BS.empty
    body     <- takeByteString
    return $! Message (Severity severity) (Category category) (Payload body)
