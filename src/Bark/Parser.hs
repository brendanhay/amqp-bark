{-# LANGUAGE OverloadedStrings #-}

module Bark.Parser (
      Message(..)
    , conduitMessage
    , conduitMessage'
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


import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit (($$), ($=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as C
import Data.Char (isSpace)
import Data.Conduit.Attoparsec (sinkParser)


conduitMessage :: MonadResource m => Conduit BS.ByteString m Message
conduitMessage =
    conduit
  where
    msg input = case parseOnly parser input of
        Right m -> m
        Left  e -> Message "error" "error" . Error $ pack e
    conduit = NeedInput push mempty
    push    = HaveOutput conduit (return ()) . msg

conduitMessage' :: MonadResource m => String -> Conduit BS.ByteString m Message
conduitMessage' = conduitParser . parser'

--
-- Internal
--

conduitParser :: (C.MonadThrow m) =>
                        A.Parser b
                     -> C.Conduit BS.ByteString m b
conduitParser p0 = C.conduitState newParser push close
  where
    newParser = A.parse (A.many1 p0)
    push _ c
        | BS.null c = return $ C.StateFinished Nothing []
    push parser c = do
        case A.feed (parser c) BS.empty of
            A.Done leftover xs
                | BS.null leftover ->
                    return $ C.StateProducing newParser xs
                | otherwise ->
                    return $ C.StateProducing (newParser . BS.append leftover) xs
            A.Fail _ contexts msg -> C.monadThrow $ C.ParseError contexts msg
            A.Partial p -> return $ C.StateProducing p []
    close parser =
        case parser BS.empty of
            A.Done _leftover xs -> return xs
            A.Fail _ contexts msg -> C.monadThrow $ C.ParseError contexts msg
            A.Partial _ -> return []

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

parser' :: String -> Parser Message
parser' delim = do
    severity <- bracketedValue
    category <- bracketedValue <|> pure defaultSeverity
    body     <- manyTill (satisfy $ const True) $ string (pack delim)
    return $! Message severity category . Payload $ BS.pack body
