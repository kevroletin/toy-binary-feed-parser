{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.State  (forM_)
import qualified Data.ByteString.Lazy as BL
import           Packet               (Packet)
import           Pcap                 (parsePcapBL)
import           Reorderer            (reorderM_)
import           SelfTest             (reorderFileAndCheck)
import           System.Environment   (getArgs, getProgName)

printHelp :: IO ()
printHelp = do
  prog <- getProgName
  mapM_ putStrLn
    [ "Possible execution modes:"
    , "  " ++ prog ++ " <file.pcap> - print messages"
    , "  " ++ prog ++ " -r <file.pcap> - reorder and print messages"
    , "  " ++ prog ++ " -c <file.pcap> - reorder packages and check "
      ++ "if result is ordered"]

printPacketsFromFile ::
     ([Packet.Packet] -> (Packet -> IO ()) -> IO b) -> FilePath -> IO b
printPacketsFromFile for file = do
  bl <- BL.readFile file
  for (parsePcapBL bl) $ putStrLn . show

main :: IO ()
main = do args <- getArgs
          case args of
            [f]       -> printPacketsFromFile forM_     f
            ["-r", f] -> printPacketsFromFile reorderM_ f
            ["-c", f] -> reorderFileAndCheck f
            _         -> printHelp
