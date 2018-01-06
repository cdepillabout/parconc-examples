{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Foldable (traverse_)
import System.Environment (getArgs)
import Text.Read (Read(readPrec))

main :: IO ()
main = do
  args <- getArgs
  let disks =
        case args of
          [] -> 20
          (h:_) -> read h
  traverse_ print $ allHanoi disks

newtype Disks = Disks { unDisks :: Int } deriving (Enum, Eq, Ord, Num)

instance Show Disks where
  show (Disks d) = show d

instance Read Disks where
  readPrec = Disks <$> readPrec

newtype Moves = Moves { unMoves :: Integer } deriving (Enum, Eq, Ord, Num)

instance Show Moves where
  show (Moves m) = show m

-- hanoi :: Disks -> Moves
-- hanoi disks
--   | disks <= 0 = 0
--   | otherwise = 1 + hanoi (disks - 1) + hanoi (disks - 1)

hanoi :: Disks -> () -> Moves
hanoi disks ()
  | disks <= 0 = 0
  | otherwise =
    let !x = hanoi (disks - 1) ()
        !y = hanoi (disks - 1) ()
        !res = 1 + x + y
    in res

allDisks :: Disks -> [Disks]
allDisks n = [1..n]

allHanoi :: Disks -> [Moves]
allHanoi n = fmap (\d -> hanoi d ()) $ allDisks n
