{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import Control.Concurrent (forkIO)
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
globalMyTMVar = unsafePerformIO $ MyTMVar <$> newTVarIO Nothing

test :: IO Int
test = do
  forkIO $ do
    say "from thread 1, sleeping 5 seconds..."
    threadDelay $ 1000 * 1000 * 5
    atomically $ putMyTMVar 3 globalMyTMVar
    say "from thread 1, put 3 in globalMyTMVar"
    say "from thread 1, sleeping 5 seconds..."
    threadDelay $ 1000 * 1000 * 5
    res <- atomically $ do
      putMyTMVar 4 globalMyTMVar
      takeMyTMVar globalMyTMVar
    say $ "from thread 1, res = " <> tshow res
  res <- atomically $ takeMyTMVar globalMyTMVar
  say $ "from thread 2, res = " <> tshow res
  pure res
