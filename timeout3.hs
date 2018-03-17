{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Control.Concurrent
import Data.Functor
import Data.Typeable
import Data.Unique

data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout unique) = "timeout " ++ show (hashUnique unique)

instance Exception Timeout

timeout :: Int -> IO a -> IO (Maybe a)
timeout time action
  | time < 0 = fmap Just action
  | time == 0 = pure Nothing
  | otherwise = do
      pid <- myThreadId
      u <- newUnique
      let ex = Timeout u
      putStrLn $ "about to start timeout function, ex = " ++ show ex
      handleJust
        (\e -> if e == ex then Just () else Nothing)
        (\() -> putStrLn "in exception handler for handleJust" *> pure Nothing)
        (bracket
          (forkIO $ do
            putStrLn $ "in forked thread, running for " ++ show time
            threadDelay time
            putStrLn "in forked thread, time over"
            throwTo pid ex
          )
          (\tid -> throwTo tid ThreadKilled)
          (\_ -> do
            putStrLn "in normal thread, about to start action"
            ret <- fmap Just action
            putStrLn "in normal thread, finished action"
            pure ret
          )
        )

main :: IO ()
main = print =<< timeout 5000000 (threadDelay 4000000 $> "hello")
