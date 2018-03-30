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

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C
import qualified Data.List                        as List

data Supply = Supply
  { price    :: {-# UNPACK #-} !Int
  , quantity :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

newtype AcceptTime = AcceptTime Int deriving (Eq, Ord)

data Message = Message
  { messageBids       :: ![Supply]
  , messageAsks       :: ![Supply]
  , messageAcceptTime :: !AcceptTime
  , messageIssueCode  :: !String
  } deriving Eq

instance Show AcceptTime where
  show at = List.intercalate ":" (fmap frm [h, m, s]) ++ "." ++ (frm u)
    where
      (h, m, s, u) = deconstructAcceptTime at
      frm x = case show x of
                [c] -> ['0', c]
                s   -> s

instance Show Message where
  show = showMessage

showMessage :: Message -> String
showMessage msg =
  List.intercalate " " $
    [ show (messageAcceptTime msg)
    , messageIssueCode msg ]
    ++ fmap suppStr (reverse $ messageBids msg)
    ++ fmap suppStr (messageAsks msg)
  where
    suppStr (Supply p q) = show q ++ "@" ++ show p

messageStartMarker :: BS.ByteString
messageStartMarker = "B6034"

deconstructAcceptTime :: AcceptTime -> (Int, Int, Int, Int)
deconstructAcceptTime (AcceptTime x) = (h, m, s, u)
  where
    (h, r1_) = x   `divMod` (60 * 60 * 100)
    (m, r2_) = r1_ `divMod` (     60 * 100)
    (s, u  ) = r2_ `divMod` (          100)

parseInt :: Int -> A.Parser Int
parseInt n = do res <- C.readInt <$> A.take n
                case res of
                  Nothing        -> fail "Number format"
                  Just (x, rest) -> if BS.null rest
                                      then return x
                                      else fail "Number format"

parseMessage :: A.Parser Message
parseMessage = do
  A.string messageStartMarker
  issueCode <- A.take 12
  _  <- A.take (3 + 2 + 7)
  bs <- parseManyPairs 5
  _  <- A.take 7
  as <- parseManyPairs 5
  _  <- A.take $ (5 + 4 * 5) * 2
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

parseAcceptTime :: A.Parser AcceptTime
parseAcceptTime = do
  h <- parseInt 2
  m <- parseInt 2
  s <- parseInt 2
  u <- parseInt 2
  return $ AcceptTime (((h * 60 + m) * 60 + s) * 100 + u)
  where
