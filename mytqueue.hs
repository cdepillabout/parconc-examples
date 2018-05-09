{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , modifyTVar'
  , newTVar
  , readTVar
  , retry
  , writeTVar
  )

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = TQueue <$> newTVar [] <*> newTVar []

readTQueue :: TQueue a -> STM a
readTQueue tqueue@(TQueue readSide writeSide) = do
  readElems <- readTVar readSide
  case readElems of
    (h:ts) -> do
      writeTVar readSide ts
      pure h
    [] -> do
      writeElems <- readTVar writeSide
      case writeElems of
        [] -> retry
        as -> do
          writeTVar readSide (reverse as)
          writeTVar writeSide []
          readTQueue tqueue

writeTQueue :: forall a. TQueue a -> a -> STM ()
writeTQueue (TQueue _ writeSide) a = modifyTVar' writeSide (a :)

-- readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
-- readEitherTChan tchanA tchanB =
--   fmap Left (readTChan tchanA) `orElse` fmap Right (readTChan tchanB)

test :: STM [Int]
test = do
  tqueue <- newTQueue
  writeTQueue tqueue (3 :: Int)
  writeTQueue tqueue 4
  res1 <- readTQueue tqueue
  -- unGetTQueue tqueue 100
  writeTQueue tqueue 5
  res2 <- readTQueue tqueue
  writeTQueue tqueue 200
  res3 <- readTQueue tqueue
  res4 <- readTQueue tqueue
  pure [res1, res2, res3, res4]

main :: IO ()
main = do
  res <- atomically test
  print res
