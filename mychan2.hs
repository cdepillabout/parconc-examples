
module Main where

import Prelude hiding (init, read)

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, mask, mask_, throwIO)
import Control.Monad
import Data.Foldable
import Data.Semigroup
import Data.Traversable

data Item a = Item a (MVar (Item a))

data Chan a = Chan
  { readSide :: MVar (MVar (Item a))
  , writeSide :: MVar (MVar (Item a))
  }

withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar mvar f = do
  a <-
    mask_ $ do
      a <- takeMVar mvar
      putMVar mvar a
      pure a
  f a

readMVar :: MVar a -> IO a
readMVar mvar = mask_ $ do
  a <- takeMVar mvar
  putMVar mvar a
  pure a

onException :: IO a -> IO b -> IO a
onException action final =
  action `catch` \e -> final *> throwIO (e :: SomeException)

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ mvar f = mask $ \restore -> do
  a <- takeMVar mvar
  newA <-
    restore (f a) `catch` \e -> do
      putMVar mvar a
      throwIO (e :: SomeException)
  putMVar mvar newA

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket init final action = mask $ \restore -> do
  a <- init
  c <- restore (action a) `onException` final a
  _ <- final a
  pure c

newChan :: IO (Chan a)
newChan = do
  init <- newEmptyMVar
  read <- newMVar init
  write <- newMVar init
  pure $ Chan read write

readChan :: Chan a -> IO a
readChan (Chan read _) = do
  mvarToItem <- takeMVar read
  Item a mvarToNextItem <- readMVar mvarToItem
  putMVar read mvarToNextItem
  pure a

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ write) a = do
  next <- newEmptyMVar
  mask_ $ do
    emptyMVarToNextItem <- takeMVar write
    putMVar emptyMVarToNextItem (Item a next)
    putMVar write next

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ write) = do
  emptyMVar <- takeMVar write
  putMVar write emptyMVar
  newRead <- newMVar emptyMVar
  pure $ Chan newRead write

readThread :: Chan Int -> IO a
readThread chan =
  forever $ do
    int <- readChan chan
    putStrLn $ "readThread: got int: " <> show int

writeThread :: MVar () -> Chan Int -> IO ()
writeThread exitMVar chan = do
  writeChan chan 1
  putStrLn "writeThread: sent int, sleeping for 3 secs"
  threadDelay $ 3 * 1000 * 1000
  writeChan chan 2
  putStrLn "writeThread: sent int, sleeping for 5 secs"
  threadDelay $ 5 * 1000 * 1000
  writeChan chan 3
  putStrLn "writeThread: sent int, ending"
  putMVar exitMVar ()

main1 :: IO ()
main1 = do
  chan <- newChan
  exitMVar <- newEmptyMVar
  void . forkIO $ readThread chan
  void . forkIO $ writeThread exitMVar chan
  takeMVar exitMVar

main2 :: IO ()
main2 = do
  chan <- newChan
  exitMVar <- newEmptyMVar
  void . forkIO $ do
    traverse_ (writeChan chan) [ 0 :: Int .. 10000 ]
    putMVar exitMVar ()
  void . forkIO . forever $ do
    i <- readChan chan
    when (i `mod` 1000 == 0) $
      putStrLn $ "i: " <> show i
  takeMVar exitMVar

main3 :: IO ()
main3 = do
  chan <- newChan
  writeChan chan "foo"
  chan2 <- dupChan chan
  writeChan chan "bar"
  writeChan chan2 "sheep"
  -- what1 <- readChan chan
  -- putStrLn $ "readChan chan: " <> what1
  -- what2 <- readChan chan
  -- putStrLn $ "readChan chan: " <> what2
  readChan chan2 >>= (putStrLn . ("readChan chan2: " <>))
  readChan chan2 >>= (putStrLn . ("readChan chan2: " <>))
  readChan chan >>= (putStrLn . ("readChan chan: " <>))
  readChan chan >>= (putStrLn . ("readChan chan: " <>))
  readChan chan >>= (putStrLn . ("readChan chan: " <>))

main :: IO ()
main = main3
