{-# LANGUAGE OverloadedStrings #-}

module Bark.Event.Incremental
    ( conduitEvent
    ) where

import Data.Attoparsec
import Data.ByteString.Char8        (pack)
import Data.Conduit          hiding (Done)
import Bark.Event.Parser
import Bark.Types

import qualified Data.ByteString as B

conduitEvent :: MonadResource m
             => String
             -> Bool
             -> Conduit B.ByteString m Event
conduitEvent delim _ = conduitParser $ parser delim

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
    push parser' c =
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

parser :: String -> Parser Event
parser delim = do
    sev  <- severity
    cat  <- category
    body <- manyTill (satisfy $ const True) $ string (pack delim)
    return $! mkEvent cat sev . Payload $ B.pack body

--
--
--

