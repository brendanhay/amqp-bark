{-# LANGUAGE OverloadedStrings #-}

module Bark.Message.Incremental (
      conduitMessage
    ) where

import Data.Attoparsec
import Data.ByteString.Char8        (pack)
import Data.Conduit          hiding (Done)
import Bark.Message.Types
import Bark.Message.Parser

import qualified Data.ByteString as BS

conduitMessage :: MonadResource m
               => String
               -> Bool
               -> Conduit BS.ByteString m Message
conduitMessage delim _ = conduitParser $ parser delim

--
-- Internal
--

conduitParser :: MonadResource m
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
            Fail _ contexts msg -> return $ StateProducing newParser []
            Partial p -> return $ StateProducing p []
    close parser' =
        case parser' BS.empty of
            Done _leftover xs   -> return xs
            Fail _ contexts msg -> return []
            Partial _           -> return []

parser :: String -> Parser Message
parser delim = do
    sev  <- severity
    cat  <- category
    body <- manyTill (satisfy $ const True) $ string (pack delim)
    return $! Message sev cat . Payload $ BS.pack body
