
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.Foldable
import Data.Maybe
import System.IO
import Text.Printf

import GetURL
import TimeIt


data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  threadId <- forkIO $ do
    r <- try action
    putMVar var r
  pure $ Async threadId var

cancel :: Async a -> IO ()
cancel (Async threadId _) = throwTo threadId ThreadKilled

wait :: Async a -> IO a
wait = either throwIO pure <=< waitCatch

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var

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

download :: String -> IO (String, ByteString)
download url = do
  r <- getURL url
  printf "finishing downloading: %s\n" url
  pure (url, r)

main2 :: IO ()
main2 = do
  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as

main3 :: IO ()
main3 = do
  hSetBuffering stdin NoBuffering
  putStrLn "starting..."
  as <- mapM (async . download) sites
  forkIO $ forever $ do
    char <- getChar
    printf "got %c\n" char
    when (char == 'q') $ do
      traverse_ cancel as
  res <- traverse waitCatch as
  let successes = length $ filter isRight res
  printf "%d succeeded out of %d\n" successes (length res)

main :: IO ()
main = main3
