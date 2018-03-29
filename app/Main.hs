{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Exception (try)
import qualified Data.List             as List
import           Data.Traversable      (traverse)
import           Data.UnixTime
import           Lib
import           System.Environment    (getArgs)
import           Control.Exception (finally)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.List             as List


import Parser
import Message
import Packet
import Control.Monad.State
import Pcap

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
  bl <- BL.readFile "/tmp/mega-file.pcap"
  forM_ (parsePcapBl bl) $ putStrLn . show

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> printPacketsFromFile f
            _   -> printHelp
