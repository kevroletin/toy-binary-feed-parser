module Packet (
  PacketTime
  , Packet(..)
  , parsePacket
  , packetToStr
) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.List             as List
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word             (Word32)
import           Message
import qualified Network.Pcap          as Pcap
import           Parser

data PacketTime = PacketTime
  { packetTimeSec  :: {-# UNPACK #-} !Word32
  , packetTimeUsec :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

data Packet = Packet
  { packetTime    :: {-# UNPACK #-} !PacketTime
  , packetMessage :: !Message
  } deriving (Show, Eq)

parsePacket :: Pcap.PktHdr -> BS.ByteString -> Maybe Packet
parsePacket hdr str =
  case parse (skipIpHeaders >> parseMessage) str of
    Nothing  -> Nothing
    Just msg -> Just $ Packet packetTime msg
  where
    packetTime = PacketTime (Pcap.hdrSeconds hdr) (Pcap.hdrUseconds hdr)
skipIpHeaders :: Parser ()
skipIpHeaders = skipCnt 42 -- IP4 + Ethernet package length

packetTimeToUtc (PacketTime s u) =
  posixSecondsToUTCTime $ (fromIntegral s) + (fromIntegral u / 1000000)

packetToStr :: Packet -> String
packetToStr (Packet time msg) =
  List.intercalate " " $
    [ show (packetTimeToUtc time)
    , messageIssueCode msg
    , show (messageAcceptTime msg)]
    ++ fmap suppStr (reverse $ messageBids msg)
    ++ fmap suppStr (messageAsks msg)
  where
    suppStr (Supply p q) = show q ++ "@" ++ show p
