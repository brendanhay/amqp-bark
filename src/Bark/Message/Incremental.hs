{-# LANGUAGE OverloadedStrings #-}

module Bark.Message.Incremental (
      conduitMessage
    ) where

import Data.Attoparsec
import Data.ByteString.Char8        (pack)
import Data.Conduit          hiding (Done)
import Bark.Message.Types
import Bark.Message.Parser

import qualified Data.ByteString as B

conduitMessage :: MonadResource m
               => String
               -> Bool
               -> Conduit B.ByteString m Message
conduitMessage delim _ = conduitParser $ parser delim

--
-- Internal
--

conduitParser :: MonadResource m
              => Parser b
              -> Conduit B.ByteString m b
conduitParser p0 = conduitState newParser push close
  where
    newParser = parse (many1 p0)
    push _ c
       | B.null c = return $ StateFinished Nothing []
    push parser' c = do
        case feed (parser' c) B.empty of
            Done leftover xs
                | B.null leftover ->
                    return $ StateProducing newParser xs
                | otherwise ->
                    return $ StateProducing (newParser . B.append leftover) xs
            Fail _ _contexts _ ->
                return $ StateProducing newParser []
            Partial p ->
                return $ StateProducing p []
    close parser' =
        case parser' B.empty of
            Done _leftover xs  -> return xs
            Fail _ _contexts _ -> return []
            Partial _          -> return []

parser :: String -> Parser Message
parser delim = do
    sev  <- severity
    cat  <- category
    body <- manyTill (satisfy $ const True) $ string (pack delim)
    return $! Message sev cat . Payload $ B.pack body
