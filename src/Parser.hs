module Parser (
  Parser
  , parse
  , parserFail
  , readCnt
  , ensureStr
  , skipCnt
  , parseInt
  , lift
) where

import           Control.Monad.State
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BL

type Parser a = StateT BS.ByteString Maybe a

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
