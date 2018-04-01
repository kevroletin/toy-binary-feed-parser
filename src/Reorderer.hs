module Reorderer (
  reorder
  , compareAcceptTime
  , AcceptTimeComparison(..)
) where

import           Config                     (Seconds (..), bufferingTime)
import           Control.Monad.State.Strict
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Message
import           Packet

newtype PacketOrdByAcceptTime =
  PacketOrdByAcceptTime
  { unOrdPacket :: Packet
  } deriving (Eq, Show)

instance Ord PacketOrdByAcceptTime where
  PacketOrdByAcceptTime a <= PacketOrdByAcceptTime b =
    if atime a /= atime b
      then atime a <= atime b
      else if ptime a /= ptime b
              then ptime a <= ptime b
              else code a <= code b
    where
      atime = messageAcceptTime . packetMessage
      ptime = packetTime
      code  = messageIssueCode  . packetMessage

type ReordererState = Set PacketOrdByAcceptTime

data AcceptTimeComparison = Mature
                          | Immature
                          deriving (Eq, Show)

compareAcceptTime :: AcceptTime -> AcceptTime
                  -> AcceptTimeComparison
compareAcceptTime (AcceptTime newer) (AcceptTime older) =
  let (Seconds th) = bufferingTime
  in if (newer - older) > th * 100
       then Mature
       else Immature

reorder' :: Set PacketOrdByAcceptTime -> [Packet] -> [Packet]
reorder' buff [] = fmap unOrdPacket $ Set.toList buff
reorder' buff (x : xs)
  | Set.null buff = reorder' (Set.singleton $ PacketOrdByAcceptTime x) xs
  | otherwise =
    let oldest     = Set.elemAt 0 buff
        atime      = messageAcceptTime . packetMessage
        oldestTime = atime $ unOrdPacket oldest
        newestTime = atime x
    in case compareAcceptTime newestTime oldestTime of
      Mature ->
        unOrdPacket oldest : reorder' (Set.delete oldest buff) (x : xs)
      Immature ->
        reorder' (Set.insert (PacketOrdByAcceptTime x) buff) xs

reorder :: [Packet] -> [Packet]
reorder = reorder' Set.empty
