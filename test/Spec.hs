{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Binary          as A
import qualified Data.Attoparsec.ByteString      as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Either                     (isLeft)
import           Data.Monoid                     ((<>))
import           Message
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Parser tests" tests)

tests =
  [ parseMessagePositive
  , parseMessageNegative ]

parseMessagePositive :: TestTree
parseMessagePositive = testCase "Valid message" $ do
  assertEqual "Only message" ans (run str)
  assertEqual "Message and trailin input" ans (run (str <> "garbage"))
  where
    run x = show <$> (A.parseOnly parseMessage) x
    ans = Right "ISINcode1234 12:34:56.78 10@9 8@7 6@5 4@3 2@1 12@11 14@13 16@15 18@17 20@19"
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
