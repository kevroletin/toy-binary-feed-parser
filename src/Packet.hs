module Packet (
  PacketTime(..)
  , Packet(..)
) where

import qualified Data.List             as List
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word             (Word32)
import           Message

data PacketTime = PacketTime
  { packetTimeSec  :: {-# UNPACK #-} !Word32
  , packetTimeUsec :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

data Packet = Packet
  { packetTime    :: {-# UNPACK #-} !PacketTime
  , packetMessage :: !Message
  } deriving Eq

instance Show Packet where
  show = packetToStr

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
