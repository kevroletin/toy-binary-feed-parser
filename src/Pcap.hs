module Pcap (
  parsePcapBl
) where

import qualified Data.Attoparsec.Binary          as A
import qualified Data.Attoparsec.ByteString      as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Lazy            as BL
import qualified Data.List                       as List
import           Data.Maybe                      (mapMaybe)
import           GHC.Stack
import           Packet

skipPcapHeader :: A.Parser ()
skipPcapHeader = do
  _ <- A.take 24
  return ()

pcapPacket :: A.Parser (PacketTime, BS.ByteString)
pcapPacket = do
  sec  <- A.anyWord32le
  usec <- A.anyWord32le
  len  <- A.anyWord32le
  _    <- A.take 4
  body <- A.take (fromIntegral len)
  return (PacketTime sec usec, body)

parsePcapRawBl :: HasCallStack
               => BL.ByteString -> [(PacketTime, BS.ByteString)]
parsePcapRawBl bs0 = do
  case AL.parse skipPcapHeader bs0 of
    AL.Fail _ _ y  -> error y
    AL.Done rest _ -> go rest
  where
    go bs
      | BL.null bs = []
      | otherwise  = case AL.parse pcapPacket bs of
                       AL.Fail _ _ y  -> error y
                       AL.Done rest x -> x : (go rest)

parsePcapBl :: BL.ByteString -> [Packet]
parsePcapBl bs = mapMaybe (uncurry parsePacket) (parsePcapRawBl bs)
