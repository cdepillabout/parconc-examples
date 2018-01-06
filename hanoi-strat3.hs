{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Par
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import Text.Read (Read(readPrec))

main :: IO ()
main = do
  args <- getArgs
  let (depth, disks) =
        case args of
          (depth' : disks' : _) -> (read depth', read disks')
          _ -> (6, 20)
  traverse_ print $ allHanoi depth disks

newtype Disks = Disks
  { unDisks :: Int
  } deriving (Enum, Eq, Ord, Num)

instance Show Disks where
  show (Disks d) = show d

instance Read Disks where
  readPrec = Disks <$> readPrec

newtype Moves = Moves
  { unMoves :: Integer
  } deriving (Enum, Eq, Ord, NFData, Num)

instance Show Moves where
  show (Moves m) = show m

hanoi :: Disks -> () -> Moves
hanoi disks ()
  | disks <= 0 = 0
  | otherwise = 1 + hanoi (disks - 1) () + hanoi (disks - 1) ()

type Depth = Int

hanoiPar :: Depth -> Disks -> Par Moves
hanoiPar depth disks
  | depth <= 0 = pure $ hanoi disks ()
  | disks <= 0 = pure 0
  | otherwise = do
      moveFirstIVar <- spawn (hanoiPar (depth - 1) (disks - 1))
      moveLastIVar <- spawn (hanoiPar (depth - 1) (disks - 1))
      moveFirst <- get moveFirstIVar
      moveLast <- get moveLastIVar
      pure $ 1 + moveFirst + moveLast

allDisks :: Disks -> [Disks]
allDisks n = [1..n]

allHanoi :: Depth -> Disks -> [Moves]
allHanoi depth n = runPar $ parMapM (hanoiPar depth) (allDisks n)
