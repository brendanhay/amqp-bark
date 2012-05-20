{-# LANGUAGE OverloadedStrings #-}

module Bark.Message.Parser
    ( severity
    , category
    ) where

import Control.Applicative
import Data.Attoparsec
import GHC.Word (Word8)
import Bark.Types

import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString       as B

severity :: Parser B.ByteString
severity = bracketedValue
{-# INLINE severity #-}

category :: Parser B.ByteString
category = bracketedValue <|> pure defaultSeverity
{-# INLINE category #-}

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = A.char8 '['
unbracket = A.char8 ']'
{-# INLINE bracket #-}
{-# INLINE unbracket #-}

bracketedValue :: Parser B.ByteString
bracketedValue = bracket *> takeTill (== 93) <* unbracket -- 93 == ']'
{-# INLINE bracketedValue #-}
