{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BL
import qualified Data.List             as List
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.UnixTime
import           Data.Word             (Word32)
import           Lib
import qualified Network.Pcap          as Pcap
import           System.Environment    (getArgs)

data Supply = Supply
  { price    :: Int
  , quantity :: Int
  } deriving (Show, Eq)

newtype AcceptTime = AcceptTime Int deriving (Eq, Ord)

data Message = Message
  { messageBids       :: ![Supply]
  , messageAsks       :: ![Supply]
  , messageAcceptTime :: !AcceptTime
  , messageIssueCode  :: String
  } deriving (Show, Eq)

data PacketTime = PacketTime
  { packetTimeSec  :: {-# UNPACK #-} !Word32
  , packetTimeUsec :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

data Packet = Packet
  { packetTime    :: {-# UNPACK #-} !PacketTime
  , packetMessage :: !Message
  } deriving (Show, Eq)

type Parser a = StateT BS.ByteString Maybe a

instance Show AcceptTime where
  show (AcceptTime x) =
    List.intercalate ":" (fmap frm [h, m, s]) ++ "." ++ (frm u)
    where
      (h, r1_) = x   `divMod` (60 * 60 * 100)
      (m, r2_) = r1_ `divMod` (     60 * 100)
      (s, u  ) = r2_ `divMod` (          100)
      frm x = case show x of
                [c] -> ['0', c]
                s   -> s

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

packetStartMarker :: BS.ByteString
packetStartMarker = "B6034"

breakPacketStartMarker :: C.ByteString -> (C.ByteString, C.ByteString)
breakPacketStartMarker = BS.breakSubstring packetStartMarker

parse :: Parser a -> BS.ByteString -> Maybe a
parse parser str = fst <$> runStateT parser str

parserFail :: Parser a
parserFail = lift Nothing

readCnt :: Int -> Parser BS.ByteString
readCnt n = do str <- get
               if BS.length str < n
                 then parserFail
                 else do let (l, r) = BS.splitAt n str
                         put r
                         return l

ensureStr :: BS.ByteString -> Parser BS.ByteString
ensureStr p = do
  str <- get
  if BS.isPrefixOf p str
    then put (BS.drop (BS.length p) str) >> return p
    else parserFail

skipCnt :: Int -> Parser ()
skipCnt n = do (_, r) <- gets (BS.splitAt n)
               put r

parseInt :: Int -> Parser Int
parseInt n = do intStr <- readCnt n
                (res, rest) <- lift $ C.readInt intStr
                if not (BS.null rest)
                  then parserFail
                  else return res

skipToQuoteMarker :: Parser ()
skipToQuoteMarker = do
  (l, r) <- gets breakPacketStartMarker
  if BS.null r
    then parserFail
    else put r

skipIpHeaders :: Parser ()
skipIpHeaders = skipCnt 42 -- IP4 + Ethernet package length

parseAcceptTime :: Parser AcceptTime
parseAcceptTime = do
  h <- readPart
  m <- readPart
  s <- readPart
  u <- readPart
  return $ AcceptTime (((h * 60 + m) * 60 + s) * 100 + u)
  where
    readPart :: Parser Int
    readPart = do str <- readCnt 2
                  lift (fst <$> C.readInt str)

parseMessage :: Parser Message
parseMessage = do
  ensureStr packetStartMarker
  issueCode <- readCnt 12 -- Issue code
  skipCnt (3 + 2 + 7)     -- Issue seq.-no. Market Status Type Total bid quote volume
  bs <- parseManyPairs 5
  skipCnt 7               -- Total ask quote volume
  as <- parseManyPairs 5
  skipCnt $ (5 + 4 * 5) * 2 -- Two sections of best bids
  atime <- parseAcceptTime
  return $ Message {
      messageBids = bs
    , messageAsks = as
    , messageAcceptTime = atime
    , messageIssueCode = C.unpack issueCode
  }
  where
    parsePair = Supply <$> parseInt 5 <*> parseInt 7
    parseManyPairs n = sequence [parsePair | _ <- [1 .. n]]

parseLine :: BS.ByteString -> Maybe Message
parseLine str = fst <$> runStateT (skipToQuoteMarker >> parseMessage) str

parsePacket :: Pcap.PktHdr -> BS.ByteString -> Maybe Packet
parsePacket hdr str =
  case parse (skipIpHeaders >> parseMessage) str of
    Nothing  -> Nothing
    Just msg -> Just $ Packet packetTime msg
  where
    packetTime = PacketTime (Pcap.hdrSeconds hdr) (Pcap.hdrUseconds hdr)

printPacketsFromFile :: String -> IO ()
printPacketsFromFile file = do
  h <- Pcap.openOffline file
  Pcap.loopBS h (-1) $ \hdr str -> do
    case (parsePacket hdr str) of
      Nothing -> pure ()
      Just x  -> putStrLn $ packetToStr x
  return ()

printHelp :: IO ()
printHelp = "Please specify file name as an argument"

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> printPacketsFromFile f
            _   -> printHelp
