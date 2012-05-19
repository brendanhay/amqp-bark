{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Data.Maybe         (fromJust)
import Network.URI        (parseURI)
import Leash.Options

import qualified Data.ByteString as B

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts

--
-- Internal
--
