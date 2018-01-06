{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, runtest ) where

import Control.Monad.Par.Scheds.Trace
  -- gives slightly better results than Control.Monad.Par with monad-par-0.3.4
import System.Environment
import qualified MapCompat as Map
import MapCompat (IntMap)
import System.Random
import Data.List
import Data.Traversable hiding (mapM)

import SparseGraph

-- -----------------------------------------------------------------------------
-- shortestPaths

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
 where
-- <<update
  update :: Graph -> Vertex -> Graph
  update g k = runPar $ do
    m <- Map.traverseWithKey (\i jmap -> spawn (return (shortmap i jmap))) g
    traverse get m
-- >>
   where
    shortmap :: Vertex -> IntMap Weight -> IntMap Weight
    shortmap i jmap = foldr shortest Map.empty vs
      where
        shortest j m =
          case (Map.lookup j jmap, (+) <$> weight g i k <*> weight g k j) of
            (Nothing, Nothing) -> m
            (Nothing, Just w ) -> Map.insert j w m
            (Just w,  Nothing) -> Map.insert j w m
            (Just w1, Just w2) -> Map.insert j (min w1 w2) m

-- -----------------------------------------------------------------------------
-- Testing

test :: [[Int]]
test  = [[  0, 999, 999,  13, 999, 999],
         [999,   0, 999, 999,   4,   9],
         [ 11, 999,   0, 999, 999, 999],
         [999,   3, 999,   0, 999,   7],
         [ 15,   5, 999,   1,   0, 999],
         [ 11, 999, 999,  14, 999,   0]]

-- correct result:
-- [ [0,  16, 999, 13, 20, 20],
--   [19,  0, 999,  5,  4,  9],
--   [11, 27,   0, 24, 31, 31],
--   [18,  3, 999,  0,  7,  7],
--   [15,  4, 999,  1,  0,  8],
--   [11, 17, 999, 14, 21,  0] ]

runtest :: [[Int]]
runtest = fromAdjMatrix (toAdjMatrix 5 (shortestPaths [0..5] (mkGraph test)))

main :: IO ()
main = do
  [h,n] <- fmap (fmap read) getArgs
  let g = mkStdGen 9999
  let (mat,vs) = randomGraph g h 100 n
  print (checksum (shortestPaths vs mat))

