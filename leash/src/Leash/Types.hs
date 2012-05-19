{-# LANGUAGE DeriveDataTypeable, MagicHash #-}

module Leash.Types
    ( Host(..)
    , Category(..)
    , Severity(..)
    , routingKey
    ) where

import Data.Char (isAsciiUpper, isAscii, isUpper)
import Data.Data
import Data.List (intercalate)
import GHC.Base

class RoutingKey a where
    frag :: a -> String

data Host = Host | String deriving (Data, Typeable, Show, Eq)

instance RoutingKey Host where
    frag Host = "*"
    frag host = show host

data Category =
      Category
    | Event
    | Create
    | Update
    | Delete
      deriving (Data, Typeable, Show, Eq)

instance RoutingKey Category where
    frag Category = "*"
    frag cat      = show cat

data Severity =
      Severity
    | Info
    | Notice
    | Warning
    | Critical
    | Error
      deriving (Data, Typeable, Show, Eq)

instance RoutingKey Severity where
    frag Severity = "*"
    frag sev      = show sev

routingKey :: Host -> Category -> Severity -> String
routingKey host cat sev | key == "*.*.*" = "#"
                        | otherwise      = key
      where
        key = normalise [frag host, frag cat, frag sev]

normalise :: [String] -> String
normalise = map unboxedToLower . intercalate "."

unboxedToLower :: Char -> Char
unboxedToLower c@(C# c#)
    | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
    | isAscii c      = c
    | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
    | otherwise      = c