{-# LANGUAGE OverloadedStrings #-}

module Bark.Message.Parser (
      severity
    , category
    ) where

import Control.Applicative
import Data.Attoparsec
import GHC.Word (Word8)
import Bark.Message.Types

import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString as BS

severity :: Parser BS.ByteString
severity = bracketedValue

category :: Parser BS.ByteString
category = bracketedValue <|> pure defaultSeverity

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'

bracketedValue :: Parser BS.ByteString
bracketedValue = bracket *> AC.takeTill (== ']') <* unbracket
