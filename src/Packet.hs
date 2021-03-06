module Packet (
  PacketTime(..)
  , Packet(..)
) where

import qualified Data.List               as List
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           Data.Word               (Word32)
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

-- packetTimeToUtc is significantly slower than packetTimeToStr (it increases
-- total execution time in no-reordering mode for more than 30%)
packetTimeToUtc (PacketTime s u) =
  posixSecondsToUTCTime $ (fromIntegral $ s * 1000000 + u) / 1000000

packetTimeToStr (PacketTime s u) =
  show s ++ '.' : show u

packetToStr :: Packet -> String
packetToStr (Packet time msg) = packetTimeToStr time ++ ' ' : show msg
