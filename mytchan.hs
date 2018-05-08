{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , newTVar
  , readTVar
  , retry
  , writeTVar
  )

data TChan a = TChan (TVar (TVar (TList a))) (TVar (TVar (TList a)))

data TList a = TNil | TCons a (TVar (TList a))


newTChan :: STM (TChan a)
newTChan = do
  emptyListTVar <- newTVar TNil
  readSide <- newTVar emptyListTVar
  writeSide <- newTVar emptyListTVar
  pure $ TChan readSide writeSide

readTChan :: TChan a -> STM a
readTChan (TChan readSide _) = do
  readListTVar <- readTVar readSide
  listVal <- readTVar readListTVar
  case listVal of
    TNil -> retry
    TCons a next -> do
      writeTVar readSide next
      pure a

writeTChan :: forall a. TChan a -> a -> STM ()
writeTChan (TChan _ writeSide) a = do
  writeListTVar <- readTVar writeSide
  endTVar <- newTVar TNil
  writeTVar writeListTVar (TCons a endTVar)
  writeTVar writeSide endTVar

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan readside _) a = do
  tailTVar <- readTVar readside
  startTVar <- newTVar (TCons a tailTVar)
  writeTVar readside startTVar

test :: STM [Int]
test = do
  chan <- newTChan
  writeTChan chan (3 :: Int)
  writeTChan chan 4
  res1 <- readTChan chan
  unGetTChan chan 100
  writeTChan chan 5
  res2 <- readTChan chan
  res3 <- readTChan chan
  writeTChan chan 200
  res4 <- readTChan chan
  res5 <- readTChan chan
  pure [res1, res2, res3, res4, res5]

main :: IO ()
main = do
  res <- atomically test
  print res
