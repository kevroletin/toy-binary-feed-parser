module Reorderer (
  reorderM_
  , compareAcceptTime
  , AcceptTimeComparison(..)
  , Seconds(..)
) where

import           Control.Monad.State
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           GHC.Stack
import           Packet
import           Message

import Debug.Trace

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
      code = messageIssueCode  . packetMessage

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

-- TODO: remove assumption
-- here we assume that state always contains at least one packet
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

reorder' :: Monad m
        => [Packet]
        -> (Packet -> m ())
        -> StateT ReordererState m ()
reorder' []       out = flushBuffer out
reorder' (x : xs) out = do
  addNewPacket x
  outputMaturePackets out (messageAcceptTime . packetMessage $ x)
  reorder' xs out

reorderM_ :: Monad m
          => [Packet] -> (Packet -> m ()) -> m ()
reorderM_ xs out = evalStateT (reorder' xs out) Set.empty
