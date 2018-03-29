module Reorderer (

) where

import           Control.Monad.State
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           GHC.Stack
import           Packet
import           Message

newtype PacketOrdByAcceptTime =
  PacketOrdByAcceptTime
  { unOrdPacket :: Packet
  } deriving (Eq, Show)

instance Ord PacketOrdByAcceptTime where
  PacketOrdByAcceptTime a <= PacketOrdByAcceptTime b =
    if time a == time b
      then code a <= code b
      else time a <= time b
    where
      time = messageAcceptTime . packetMessage
      code = messageAcceptTime . packetMessage

type ReordererState = Set PacketOrdByAcceptTime

data AcceptTimeComparison = Mature
                          | Immature
                          deriving (Eq, Show)

newtype Seconds = Seconds Int deriving (Eq, Show)

compareAcceptTime :: AcceptTime -> AcceptTime -> Seconds
                  -> AcceptTimeComparison
compareAcceptTime (AcceptTime newer) (AcceptTime older) (Seconds th) =
  if (newer - older) > th * 100
    then Mature
    else Immature

flushBuffer :: Monad m => (Packet -> m ()) -> StateT ReordererState m ()
flushBuffer out = do xs <- get
                     lift $ mapM_ (out . unOrdPacket) xs

addNewPacket :: Monad m => Packet -> StateT ReordererState m ()
addNewPacket p = modify $ Set.insert (PacketOrdByAcceptTime p)

outputMaturePackets :: (HasCallStack, Monad m)
                    => (Packet -> m ())
                    -> AcceptTime
                    -> StateT ReordererState m ()
outputMaturePackets out newestTime = do
  oldest <- gets (Set.elemAt 0)
  let oldestTime = messageAcceptTime . packetMessage . unOrdPacket $ oldest
  case compareAcceptTime newestTime oldestTime (Seconds 3) of
    Mature   -> do
      lift $ out (unOrdPacket oldest)
      modify (Set.delete oldest)
      outputMaturePackets out newestTime
    Immature -> return ()

reorder :: Monad m
        => m (Maybe Packet)
        -> (Packet -> m ())
        -> StateT ReordererState m ()
reorder inp out = do
  p0 <- lift $ inp
  case p0 of
    Nothing -> flushBuffer out
    Just  p -> do
      addNewPacket p
      outputMaturePackets out (messageAcceptTime . packetMessage $ p)
