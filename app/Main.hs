{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.MVar
import           Control.Exception (try)
import qualified Data.List             as List
import           Data.Traversable      (traverse)
import           Data.UnixTime
import           Lib
import qualified Network.Pcap          as Pcap
import           System.Environment    (getArgs)
import           Control.Exception (finally)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.List             as List

import Parser
import Message
import Packet
import Control.Monad.State

breakMessageStartMarker :: C.ByteString -> (C.ByteString, C.ByteString)
breakMessageStartMarker = BS.breakSubstring messageStartMarker

skipToQuoteMarker :: Parser ()
skipToQuoteMarker = do
  (l, r) <- gets breakMessageStartMarker
  if BS.null r
    then parserFail
    else put r

parseLine :: BS.ByteString -> Maybe Message
parseLine str = fst <$> runStateT (skipToQuoteMarker >> parseMessage) str

printHelp :: IO ()
printHelp = print "Please specify file name as an argument"

printPacketsFromFile :: String -> IO ()
printPacketsFromFile file = do
  h <- Pcap.openOffline file
  Pcap.loopBS h (-1) $ \hdr str -> do
    case (parsePacket hdr str) of
      Nothing -> pure ()
      Just x  -> putStrLn $ packetToStr x
  return ()

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> printPacketsFromFile f
            _   -> printHelp
