
import Control.Concurrent hiding (forkFinally)
-- import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import System.IO
import Text.Printf

import GetURL
import TimeIt


data Async a = Async ThreadId (TMVar (Either SomeException a))

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action f =
  mask $ \restore ->
    forkIO $ do
      res <- try (restore action)
      f res

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  threadId <- forkFinally action (atomically . putTMVar var)
  pure $ Async threadId var

cancel :: Async a -> IO ()
cancel (Async threadId _) = throwTo threadId ThreadKilled

wait :: Async a -> IO a
wait = atomically . waitSTM

waitSTM :: Async a -> STM a
waitSTM = waitCatchSTM >=> either throwSTM pure

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitEitherSTM :: Async a -> Async b -> STM (Either a b)
waitEitherSTM async1 async2 =
  fmap Left (waitSTM async1) `orElse` fmap Right (waitSTM async2)

waitAnySTM :: [Async a] -> STM a
waitAnySTM = foldr (orElse . waitSTM) retry

waitAny :: [Async a] -> IO a
waitAny = atomically . waitAnySTM

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.yahoo.co.jp"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

-- main1 :: IO ()
-- main1 = do
--   as <- mapM (async . timeDownload) sites
--   mapM_ wait as

download :: String -> IO (String, ByteString)
download url = do
  r <- getURL url
  printf "finishing downloading: %s (%d bytes)\n" url (B.length r)
  pure (url, r)

main2 :: IO ()
main2 = do
  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as

-- main3 :: IO ()
-- main3 = do
--   hSetBuffering stdin NoBuffering
--   putStrLn "starting..."
--   as <- mapM (async . download) sites
--   forkIO $ forever $ do
--     char <- getChar
--     printf "got %c\n" char
--     when (char == 'q') $ do
--       traverse_ cancel as
--   res <- traverse waitCatch as
--   let successes = length $ filter isRight res
--   printf "%d succeeded out of %d\n" successes (length res)

-- main4 :: IO ()
-- main4 = mask $ \restore -> do
--   maskingState <- getMaskingState
--   putStrLn $ "masking state: " <> show maskingState
--   int <- restore (pure 1) :: IO Int
--   char <- restore (pure 'c') :: IO Char
--   putStrLn $ "int: " <> show int
--   putStrLn $ "char: " <> show char

-- myMask :: ((IO a -> IO a) -> IO b) -> IO b
-- myMask = mask

-- main5 :: IO ()
-- main5 = myMask $ \restore -> do
--   int <- restore (pure 1) :: IO Int
--   -- Can't do the following because the type of myMask is not general enough.
--   -- mask is sufficiently general.
--   -- char <- restore (pure 'c') :: IO Char
--   putStrLn $ "int: " <> show int
--   -- putStrLn $ "char: " <> show char

main :: IO ()
main = main2
