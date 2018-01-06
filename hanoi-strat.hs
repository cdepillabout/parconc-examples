{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Parallel.Strategies
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
  traverse_ print $ allHanoiStrat disks

newtype Disks = Disks { unDisks :: Int } deriving (Enum, Eq, Ord, Num)

instance Show Disks where
  show (Disks d) = show d

instance Read Disks where
  readPrec = Disks <$> readPrec

newtype Moves = Moves { unMoves :: Integer } deriving (Enum, Eq, Ord, Num)

instance Show Moves where
  show (Moves m) = show m

hanoi :: Disks -> Moves
hanoi disks
  | disks <= 0 = 0
  | otherwise = 1 + hanoi (disks - 1) + hanoi (disks - 1)

allDisks :: Disks -> [Disks]
allDisks n = [1..n]

allHanoiStrat :: Disks -> [Moves]
allHanoiStrat n = fmap hanoi (allDisks n) `using` parList rseq
