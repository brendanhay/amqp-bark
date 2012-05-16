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
{-# INLINE severity #-}

category :: Parser BS.ByteString
category = bracketedValue <|> pure defaultSeverity
{-# INLINE category #-}

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'
{-# INLINE bracket #-}
{-# INLINE unbracket #-}

bracketedValue :: Parser BS.ByteString
bracketedValue = bracket *> takeTill (== 93) <* unbracket -- 93 == ']'
{-# INLINE bracketedValue #-}
