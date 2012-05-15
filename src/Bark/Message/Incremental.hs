{-# LANGUAGE OverloadedStrings #-}

module Bark.Message.Incremental (
      Message(..)
    , conduitMessage
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString.Char8 (pack)
import Data.Conduit hiding   (Done)
import GHC.Word              (Word8)
import Bark.Message.Types

import qualified Data.Attoparsec.Char8   as AC
import qualified Data.ByteString         as BS
import qualified Data.Conduit.Attoparsec as C

conduitMessage :: MonadResource m => String -> Conduit BS.ByteString m Message
conduitMessage = conduitParser . parser

--
-- Internal
--

conduitParser :: (MonadThrow m)
              => Parser b
              -> Conduit BS.ByteString m b
conduitParser p0 = conduitState newParser push close
  where
    newParser = parse (many1 p0)
    push _ c
        | BS.null c = return $ StateFinished Nothing []
    push parser' c = do
        case feed (parser' c) BS.empty of
            Done leftover xs
                | BS.null leftover ->
                    return $ StateProducing newParser xs
                | otherwise ->
                    return $ StateProducing (newParser . BS.append leftover) xs
            Fail _ contexts msg -> monadThrow $ C.ParseError contexts msg
            Partial p -> return $ StateProducing p []
    close parser' =
        case parser' BS.empty of
            Done _leftover xs -> return xs
            Fail _ contexts msg -> monadThrow $ C.ParseError contexts msg
            Partial _ -> return []

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'

bracketedValue :: Parser BS.ByteString
bracketedValue = bracket *> AC.takeTill (== ']') <* unbracket

parser :: String -> Parser Message
parser delim = do
    severity <- bracketedValue
    category <- bracketedValue <|> pure defaultSeverity
    body     <- manyTill (satisfy $ const True) $ string (pack delim)
    return $! Message severity category . Payload $ BS.pack body
