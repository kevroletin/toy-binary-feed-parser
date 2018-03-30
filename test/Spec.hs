{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Writer
import qualified Data.Attoparsec.Binary          as A
import qualified Data.Attoparsec.ByteString      as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Either                     (isLeft)
import           Data.Monoid                     ((<>))
import           GHC.Word
import           Message
import           Packet
import           Reorderer
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Parser tests" tests)

tests =
  [ parseMessagePositive
  , parseMessageNegative
  , reordererComparisons
  , reordererTest
  , reordererTestSortingField
  , reordererTestSortingWindow
  ]

parseMessagePositive :: TestTree
parseMessagePositive = testCase "Valid message" $ do
  assertEqual "Only message" ans (run str)
  assertEqual "Message and trailin input" ans (run (str <> "garbage"))
  where
    run x = show <$> (A.parseOnly parseMessage) x
    ans = Right "12:34:56.78 ISINcode1234 10@9 8@7 6@5 4@3 2@1 12@11 14@13 16@15 18@17 20@19"
    str = "B6034ISINcode1234............000010000002000030000004000050000006000070000008000090000010.......000110000012000130000014000150000016000170000018000190000020..................................................123456780."

parseMessageNegative :: TestTree
parseMessageNegative = testCase "Invalid message" $ do
  assertBool "Invalid magic" (isLeft $ run wrongMagic)
  assertBool "Invalid number characters" (isLeft $ run wrongNum)
  assertBool "Invalid arrival time characters" (isLeft $ run wrongTime)
  where
    run = A.parseOnly parseMessage
    wrongMagic = "xxxxxISINcode1234............000010000002000030000004000050000006000070000008000090000010.......000110000012000130000014000150000016000170000018000190000020..................................................123456780."
    wrongNum = "B6034ISINcode1234............0000x0000002000030000004000050000006000070000008000090000010.......000110000012000130000014000150000016000170000018000190000020..................................................123456780."
    wrongTime = "B6034ISINcode1234............000010000002000030000004000050000006000070000008000090000010.......000110000012000130000014000150000016000170000018000190000020..................................................1234x6780."

makePacket :: Word32 -> Int -> String -> Packet
makePacket netTime msgTime msgId =
  Packet
  { packetTime = PacketTime 0 netTime
  , packetMessage =
    Message
    { messageBids = [Supply x x | x <- [1 .. 5]]
    , messageAsks = [Supply x x | x <- [1 .. 5]]
    , messageAcceptTime = AcceptTime msgTime
    , messageIssueCode = msgId
    }
  }

reordererComparisons :: TestTree
reordererComparisons = testCase "Arrival time comparison" $ do
  assertEqual "Mature" Mature $ compareAcceptTime (AcceptTime 301) (AcceptTime 0) (Seconds 3)
  assertEqual "Immature" Immature $ compareAcceptTime (AcceptTime 100) (AcceptTime 0) (Seconds 3)
  assertEqual "Flipped arguments" Immature $ compareAcceptTime (AcceptTime 0) (AcceptTime 300) (Seconds 3)

reordererTest :: TestTree
reordererTest = testCase "Basic work of reorderer" $ do
  assertEqual "Already sorted" sorted $ run sorted
  assertEqual "Reversed"       sorted $ run (reverse sorted)
  assertEqual "Shuffled"       sorted $ run [m5, m1, m3, m2, m6, m4]
  assertEqual "Single elem"    [m1]   $ run [m1]
  assertEqual "Empty input"    []     $ run []
  where
    run xs = execWriter $ reorderM_ xs (tell . (:[]))
    sorted = [m1, m2, m3, m4, m5, m6]
    m1 = makePacket 1 1 "m1"
    m2 = makePacket 1 2 "m2"
    m3 = makePacket 1 3 "m3"
    m4 = makePacket 1 4 "m4"
    m5 = makePacket 1 5 "m5"
    m6 = makePacket 1 6 "m6"

reordererTestSortingField :: TestTree
reordererTestSortingField = testCase "Ensure reorderer sorts by message accept time" $ do
  assertEqual "Already sorted" sorted   $ run sorted
  assertEqual "Reversed"       sorted   $ run (reverse sorted)
  assertEqual "Shuffled"       sorted   $ run [m5, m1, m3, m2, m6, m4]
  assertEqual "Distant elems"  [m1, m6] $ run [m6, m1]
  assertEqual "Single elem"    [m1]     $ run [m1]
  assertEqual "Empty input"    []       $ run []
  where
    run xs = execWriter $ reorderM_ xs (tell . (:[]))
    sorted = [m1, m2, m3, m4, m5, m6]
    m1 = makePacket 1 1 "m6"
    m2 = makePacket 3 2 "m5"
    m3 = makePacket 2 3 "m4"
    m4 = makePacket 6 4 "m3"
    m5 = makePacket 4 5 "m2"
    m6 = makePacket 5 6 "m1"

reordererTestSortingWindow :: TestTree
reordererTestSortingWindow = testCase "Ensure reorderer sorts only within 3 sec window" $ do
  assertEqual "Switch adjacent messages"  [m1, m2] (run [m2, m1])

  -- This is a subtle moment. This test checks situation which guaranteed to never 
  -- happen just to demonstrate the algorythm behaviour (which may seem confusing).
  assertEqual "Buffer and sort distant if not other messages flushed buffer"
    [m1, m6] (run [m6, m1])

  -- Here the situation is similar to the previous case except for that m6
  -- causes flush of m2 and hence when m1 arrives it will not be reordered with m2.
  assertEqual "Should not switch distant" [m2, m1, m6] (run [m2, m6, m1])

  where
    run xs = execWriter $ reorderM_ xs (tell . (:[]))
    sorted = [m1, m2, m3, m4, m5, m6]
    m1 = makePacket 1   0 "m1"
    m2 = makePacket 1 100 "m2"
    m3 = makePacket 1 200 "m3"
    m4 = makePacket 1 300 "m4"
    m5 = makePacket 1 400 "m5"
    m6 = makePacket 1 500 "m6"
