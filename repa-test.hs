{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Data.Array.Repa as Repa
import Data.Monoid
import Data.Time.Clock
import System.Random

x = 5
y = 4000000

main :: IO ()
main = do
  let gen = mkStdGen 1
      -- inputNums1 = take (x * y) $ randomRs (1 :: Int, 10) gen
      -- inputNums2 = take (x * y) $ randomRs (11 :: Int, 20) gen
      inputNums1 = [1 .. x * y]
      inputNums2 = [1 .. x * y]
  putStrLn "before evaluate inputNums1"
  currTime1 <- getCurrentTime
  !_ <- evaluate $ length inputNums1
  !_ <- evaluate $ length inputNums2
  currTime2 <- getCurrentTime
  putStrLn $ "after evaluate: " <> show (diffUTCTime currTime2 currTime1)
  let arr1 = fromListUnboxed (ix2 x y) inputNums1
      arr2 = fromListUnboxed (ix2 x y) inputNums2
      zippedTogether = Repa.zipWith (+) arr1 arr2
      -- succed = Repa.map succ zippedTogether
  currTime3 <- getCurrentTime
  -- resArr1 <- foldP (+) 0 arr1
  -- resArr2 <- foldP (+) 0 arr2
  -- resZipped <- foldP (+) 0 zippedTogether
  -- (resArr1PlusArr2 :: Array U DIM1 Int) <- computeP $ Repa.zipWith (+) resArr1 resArr2
  let !resArr1 = foldS (+) 0 arr1
      !resArr2 = foldS (+) 0 arr2
      !resZipped = foldS (+) 0 zippedTogether
      !(resArr1PlusArr2 :: Array U DIM1 Int) = computeS $ Repa.zipWith (+) resArr1 resArr2
  currTime4 <- getCurrentTime
  putStrLn $ "after computation: " <> show (diffUTCTime currTime4 currTime3)
  -- (resSucced :: Array U DIM2 Int) <- computeP succed
  p "resArr1" resArr1
  p "resArr2" resArr2
  p "resZipped" resZipped
  p "resArr1PlusArr2" resArr1PlusArr2
  -- p "resSucced" resSucced

p :: Show a => String -> a -> IO ()
p msg a = putStrLn $ msg <> ": " <> show a
