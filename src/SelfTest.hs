module SelfTest (
  reorderFileAndCheck
) where

import           Control.Monad.Writer
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList           as DList
import           Data.List            (find)
import           Message              (messageAcceptTime, messageIssueCode)
import           Packet               (Packet(..))
import           Pcap                 (parsePcapBL)
import           Reorderer            (reorderM_)

checkOrder :: [Packet] -> Maybe (Packet, Packet, Int)
checkOrder xs = find ooo (pairs xs)
  where
    packetId   = messageIssueCode . packetMessage
    packetTime = messageAcceptTime . packetMessage

    ooo (old, new, idx) = packetTime old > packetTime new

    pairs :: [a] -> [(a, a, Int)]
    pairs [] = []
    pairs xs = zip3 xs (tail xs) [1..]

reorderFileAndCheck :: FilePath -> IO ()
reorderFileAndCheck file = do
  bl <- BL.readFile file
  packets <- execWriterT $ reorderM_ (parsePcapBL bl) (tell . DList.singleton)

  case checkOrder (DList.toList packets) of
    Nothing -> putStrLn "Good"
    Just (a, b, i) ->
      putStrLn ("Out of order #" ++ show i ++ ": " ++ packetId a ++ ", " ++ packetId b)
      where
        packetId   = messageIssueCode . packetMessage
