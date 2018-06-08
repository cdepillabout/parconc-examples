{-# LANGUAGE ScopedTypeVariables #-}
import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import System.IO
import Text.Printf
import Control.Exception

-- <<main
main = do
  tid <- forkIO $ do
    printf "inside forkIO\n"
    withSocketsDo $ do
      printf "inside withSocketsDo\n"
      sock <- listenOn (PortNumber 44444)              -- <1>
      printf "Listening on port %d\n" (44444 :: Int)
      forever $ do                                                   -- <2>
        printf "inside forever\n"
        (handle, host, port) <- accept sock                         -- <3>
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (talk handle) (\res -> printf "in finally, res: %s\n" (show res) >> hClose handle)             -- <4>
  threadDelay 10000000000 `catch` (\(e :: SomeException) -> printf "got exception in main thread: %s\n" (show e) >> throwTo tid e)

-- <<talk
talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering                                -- <1>
  loop                                                         -- <2>
 where
  loop = do
    line <- hGetLine h                                         -- <3>
    if line == "end"                                           -- <4>
       then hPutStrLn h ("Thank you for using the " ++         -- <5>
                         "Haskell doubling service.")
       else do hPutStrLn h (show (2 * (read line :: Integer))) -- <6>
               loop                                            -- <7>
-- >>
