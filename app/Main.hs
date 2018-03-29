{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.State  (forM_)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Pcap
import           System.Environment   (getArgs)

printHelp :: IO ()
printHelp = print "Please specify file name as an argument"

printPacketsFromFile :: String -> IO ()
printPacketsFromFile file = do
  bl <- BL.readFile file
  forM_ (parsePcapBl bl) $ putStrLn . show

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> printPacketsFromFile f
            _   -> printHelp
