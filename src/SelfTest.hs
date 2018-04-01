module SelfTest (
  reorderFileAndCheck
) where

import qualified Data.ByteString.Lazy as BL
import           Data.List            (find)
import           Message              (messageAcceptTime, messageIssueCode)
import           Packet               (Packet (..))
import           Pcap                 (parsePcapBL)
import           Reorderer            (reorder)

checkOrder :: [Packet] -> Maybe (Packet, Packet, Int)
checkOrder xs = find ooo (pairs xs)
  where
    packetTime = messageAcceptTime . packetMessage

    ooo (old, new, idx) = packetTime old > packetTime new

    pairs :: [a] -> [(a, a, Int)]
    pairs [] = []
    pairs xs = zip3 xs (tail xs) [1..]

reorderFileAndCheck :: FilePath -> IO ()
reorderFileAndCheck file = do
  bl <- BL.readFile file
  case checkOrder (reorder $ parsePcapBL bl) of
    Nothing -> putStrLn "Good"
    Just (a, b, i) ->
      let packetId = messageIssueCode . packetMessage
      in putStrLn ("Out of order #" ++ show i ++ ": " ++
                   packetId a ++ ", " ++ packetId b)
