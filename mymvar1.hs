
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Foldable
import Text.Printf

import GetURL
import TimeIt


data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- try action; putMVar var r)
  return (Async var)

wait :: Async a -> IO a
wait = either throwIO pure <=< waitCatch

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main1 :: IO ()
main1 = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither async1 async2 = do
  var <- newEmptyMVar
  forkIO $ wait async1 >>= putMVar var . Left
  forkIO $ wait async2 >>= putMVar var . Right
  takeMVar var

waitAny :: [Async a] -> IO a
waitAny asyncs = do
  var <- newEmptyMVar
  for_ asyncs $ \asyc -> forkIO $ wait asyc >>= putMVar var
  takeMVar var

main2 :: IO ()
main2 = do
  let
    download url = do
      r <- getURL url
      return (url, r)

  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as

main :: IO ()
main = main2
