{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import Control.Concurrent
import Control.Concurrent.STM (retry)
import Control.Exception
import Data.Functor
import System.IO.Unsafe

main :: IO ()
main = pure ()

bad :: IO a
bad = atomically retry

newtype MyTMVar a = MyTMVar (TVar (Maybe a))

newEmptyMyTMVar :: STM (MyTMVar a)
newEmptyMyTMVar = MyTMVar <$> newTVar Nothing

takeMyTMVar :: MyTMVar a -> STM a
takeMyTMVar (MyTMVar tvar) = do
  res <- readTVar tvar
  case res of
    Nothing -> retry
    Just a -> writeTVar tvar Nothing $> a

putMyTMVar :: a -> MyTMVar a -> STM ()
putMyTMVar a (MyTMVar tvar) = do
  res <- readTVar tvar
  case res of
    Nothing -> writeTVar tvar (Just a)
    Just _ -> retry

globalMyTMVar :: MyTMVar Int
globalMyTMVar = unsafePerformIO $ atomically newEmptyMyTMVar

test :: IO Int
test = do
  _ <- evaluate globalMyTMVar
  forkIO $ do
    atomically $ putMyTMVar 3 globalMyTMVar
    res <- atomically $ do
      putMyTMVar 4 globalMyTMVar
      takeMyTMVar globalMyTMVar
    say $ "from thread 1, res = " <> tshow res
  res <- atomically $ takeMyTMVar globalMyTMVar
  say $ "from thread 2, res = " <> tshow res
  pure res

