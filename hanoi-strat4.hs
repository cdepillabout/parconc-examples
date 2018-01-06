{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Monad.Par.Class
import Control.Monad.Par.IO
import Control.Monad.IO.Class
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
  allHanoi depth disks

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
  | otherwise =
    let !x = hanoi (disks - 1) ()
        !y = hanoi (disks - 1) ()
        !res = 1 + x + y
    in res

type Depth = Int

hanoiPar :: Depth -> Disks -> ParIO ()
hanoiPar depth' disks' = do
  moves <- go depth' disks'
  liftIO $ print moves
  where
    go :: Depth -> Disks -> ParIO Moves
    go depth disks
      | depth <= 0 = pure $! hanoi disks ()
      | disks <= 0 = pure 0
      | otherwise = do
          moveFirstIVar <- spawn (go (depth - 1) (disks - 1))
          moveLastIVar <- spawn (go (depth - 1) (disks - 1))
          moveFirst <- get moveFirstIVar
          moveLast <- get moveLastIVar
          pure $! 1 + moveFirst + moveLast

allDisks :: Disks -> [Disks]
allDisks n = [1..n]

allHanoi :: Depth -> Disks -> IO ()
allHanoi depth n = runParIO $ parMapM_ (hanoiPar depth) (allDisks n)

-- parMapM :: (Traversable t, NFData b, ParFuture iv p) => (a -> p b) -> t a -> p (t b)
-- parMapM f xs = mapM (spawn . f) xs >>= mapM get

parMapM_ :: (Traversable t) => (a -> ParIO ()) -> t a -> ParIO ()
-- parMapM_ f xs = do
--   ivars <- traverse (spawn . f) xs
--   traverse get ivars
parMapM_ f xs = traverse_ (spawn . f) xs
