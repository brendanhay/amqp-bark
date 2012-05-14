{-# LANGUAGE OverloadedStrings #-}

module Bark.Parser (
      Message(..)
    , conduitMessage
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString.Char8 (pack)
import Data.Conduit
import Data.Monoid           (mempty)
import GHC.Word              (Word8)
import Bark.Message

import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString       as BS

conduitMessage :: MonadResource m => Conduit BS.ByteString m Message
conduitMessage =
    conduit
  where
    parse input = case parseOnly parser input of
        Right m -> m
        Left  e -> Message "error" "error" . Error $ pack e
    conduit = NeedInput push mempty
    push    = HaveOutput conduit (return ()) . parse

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'

bracketedValue :: Parser BS.ByteString
bracketedValue = bracket *> AC.takeTill (== ']') <* unbracket

parser :: Parser Message
parser = do
    severity <- bracketedValue
    category <- bracketedValue <|> pure defaultSeverity
    body     <- takeByteString
    return $! Message severity category (Payload body)
