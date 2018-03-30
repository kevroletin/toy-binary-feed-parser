module Config where

newtype Seconds = Seconds Int deriving (Eq, Show)

bufferingTime = Seconds 3
