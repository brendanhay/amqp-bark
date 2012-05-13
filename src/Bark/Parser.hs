{-# LANGUAGE OverloadedStrings #-}

module Bark.Parser (
      Message(..)
    , conduitMessage
    , conduitMessage'
    ) where

import Prelude hiding (null)
import Control.Applicative hiding (empty)
import Data.Attoparsec
import Data.ByteString.Char8
import Data.ByteString.Internal   (c2w)
import Data.Conduit
import Data.Monoid                (mempty)
import GHC.Word (Word8)
import Data.Conduit.Attoparsec (ParseError(..), sinkParser)

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.Char8 as AC

data Message = Message
    { msgSeverity :: !ByteString
    , msgCategory :: !ByteString
    , msgBody     :: !ByteString
    } deriving (Eq, Show)

conduitMessage :: MonadResource m
               => Conduit ByteString m Message
conduitMessage =
    conduit
  where
    conduit     = NeedInput push mempty
    push        = HaveOutput conduit (return ()) . parse
    parse input = case parseOnly message input of
        Right msg -> msg
        Left  err -> Message (pack "ERROR") (pack "error") (pack err)

conduitMessage' :: MonadThrow m
               => Conduit ByteString m Message
conduitMessage' = conduitParser message'

conduitParser :: MonadThrow m
              => Parser b
              -> Conduit ByteString m b
conduitParser p0 = conduitState newParser push close
  where
    newParser = parse (many1 p0)
    push _ c
        | null c = return $ StateFinished Nothing []
    push parser c = do
        case feed (parser c) empty of
            AC.Done leftover xs
                | null leftover ->
                    return $ StateProducing newParser xs
                | otherwise ->
                    return $ StateProducing (newParser . append leftover) xs
            AC.Fail _ contexts msg -> monadThrow $ ParseError contexts msg
            AC.Partial p -> return $ StateProducing p []
    close parser =
        case parser empty of
            AC.Done _leftover xs -> return xs
            AC.Fail _ contexts msg -> monadThrow $ ParseError contexts msg
            AC.Partial _ -> return [] -- A partial parse when closing is not an error

--
-- Internal
--

bracket, unbracket :: Parser Word8
bracket   = AC.char8 '['
unbracket = AC.char8 ']'

fromBrackets :: Parser ByteString
fromBrackets = bracket *> AC.takeTill (== ']') <* unbracket

-- parse exact
message :: Parser Message
message = do
    severity <- fromBrackets
    category <- fromBrackets <|> pure empty
    body     <- takeByteString
    return $! Message severity category body

-- parse incremental
message' :: Parser Message
message' = do
    severity <- fromBrackets
    category <- fromBrackets <|> pure empty
    body     <- manyTill (satisfy $ const True) $ AC.char8 '\n'
    return $! Message severity category (BS.pack body)
