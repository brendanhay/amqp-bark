{-# LANGUAGE ExistentialQuantification #-}

module Bark.Conduit (
      Delimiter(..)
    , fromString
    , conduitSplit
    , conduitHandle
    ) where

import Control.Monad.IO.Class   (liftIO)
import Data.ByteString.Char8    (pack)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Unsafe   (unsafeTake, unsafeDrop, unsafeTail)
import Data.Conduit
import Data.Word                (Word8)
import System.IO                (Handle)

import qualified Data.ByteString as BS

data AnyDelimiter = forall a. Delimiter a => AnyDelimiter a

class Delimiter a where
    split :: a -> Bool -> BS.ByteString -> ([BS.ByteString], BS.ByteString)

instance Delimiter AnyDelimiter where
    split (AnyDelimiter d) = split d

instance Delimiter Word8 where
    split d drop bstr = case BS.elemIndex d bstr of
        Nothing -> ([], bstr)
        Just n  -> ([unsafeTake (n + pad) bstr], unsafeDrop (n + 1) bstr)
          where
            pad = if drop then 0 else 1

instance Delimiter BS.ByteString where
    split d drop bstr = search 0 bstr
      where
        search n s | BS.null s = ([], bstr)
                   | prefix    = ([unsafeTake (n + pad) bstr], unsafeDrop len s)
                   | otherwise = search (n + 1) (unsafeTail s)
          where
            prefix = d `BS.isPrefixOf` s
            len    = BS.length d
            pad    = if drop then 0 else len


fromString :: String -> AnyDelimiter
fromString str | (length str) > 1 = AnyDelimiter $ pack str
               | otherwise        = AnyDelimiter . c2w $ head str

conduitSplit :: (Delimiter d, Monad m)
             => d
             -> Conduit BS.ByteString m BS.ByteString
conduitSplit delim =
    conduitState BS.empty push close
  where
    push state input = return $ StateProducing state' res
      where
        (state', res) | BS.null rest = (buffer, [])
                      | null matches = (buffer, [])
                      | otherwise    = (rest, matches)
          where
            buffer          = BS.append state input
            (matches, rest) = split delim True buffer
    close state = return [state]

conduitHandle :: MonadResource m
              => Handle
              -> Conduit BS.ByteString m BS.ByteString
conduitHandle handle =
    conduit
  where
    conduit = NeedInput push close
    push bstr = PipeM
        (liftIO (BS.hPut handle bstr) >> return conduit)
        (return ())
    close = return ()

--
-- Internal
--

breakSubstring :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
breakSubstring d bstr = splitIndices (findIndices d bstr) bstr

findIndices :: BS.ByteString -> BS.ByteString -> [Int]
findIndices d bstr | BS.null d = [0..BS.length bstr]
                   | otherwise = offset $ search 0 bstr
  where
    offset = map (+ (BS.length d))

    search a b | a `seq` b `seq` False = undefined
    search n s | BS.null s             = []
               | d `BS.isPrefixOf` s   = n : continue
               | otherwise             = continue
      where
        continue = search (n + 1) (unsafeTail s)

splitIndices :: [Int] -> BS.ByteString -> [BS.ByteString]
splitIndices [] _        = []
splitIndices (x:xs) bstr = left : splitIndices xs right
  where
    (left, right) = BS.splitAt x bstr
