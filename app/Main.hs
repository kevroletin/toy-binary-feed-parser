module Main where

import           Control.Monad.State  (forM_)
import qualified Data.ByteString.Lazy as BL
import           Packet               (Packet)
import           Pcap                 (parsePcapBL)
import           Reorderer            (reorder)
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

printPacketsFromFile :: ([Packet] -> [Packet]) -> FilePath -> IO ()
printPacketsFromFile trans file = do
  bl <- BL.readFile file
  mapM_ (putStrLn . show) (trans $ parsePcapBL bl)

main :: IO ()
main = do args <- getArgs
          case args of
            [f]       -> printPacketsFromFile id      f
            ["-r", f] -> printPacketsFromFile reorder f
            ["-c", f] -> reorderFileAndCheck f
            _         -> printHelp
