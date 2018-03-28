{-# LANGUAGE OverloadedStrings #-}

module Message (
  AcceptTime(..)
  , Message(..)
  , Supply(..)
  , deconstructAcceptTime
  , parseMessage
  , parseAcceptTime
  , messageStartMarker
) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.List             as List
import           Parser

data Supply = Supply
  { price    :: Int
  , quantity :: Int
  } deriving (Show, Eq)

newtype AcceptTime = AcceptTime Int deriving (Eq, Ord)

data Message = Message
  { messageBids       :: ![Supply]
  , messageAsks       :: ![Supply]
  , messageAcceptTime :: !AcceptTime
  , messageIssueCode  :: !String
  } deriving (Show, Eq)

instance Show AcceptTime where
  show at = List.intercalate ":" (fmap frm [h, m, s]) ++ "." ++ (frm u)
    where
      (h, m, s, u) = deconstructAcceptTime at
      frm x = case show x of
                [c] -> ['0', c]
                s   -> s

messageStartMarker :: BS.ByteString
messageStartMarker = "B6034"

deconstructAcceptTime :: AcceptTime -> (Int, Int, Int, Int)
deconstructAcceptTime (AcceptTime x) = (h, m, s, u)
  where
    (h, r1_) = x   `divMod` (60 * 60 * 100)
    (m, r2_) = r1_ `divMod` (     60 * 100)
    (s, u  ) = r2_ `divMod` (          100)

parseMessage :: Parser Message
parseMessage = do
  ensureStr messageStartMarker
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
