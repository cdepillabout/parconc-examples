module WindowManager2 where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

data Window = Window Int deriving (Eq, Ord, Show)

data Desktop = Desktop Int deriving (Eq, Ord, Show)

type Display = Map Desktop (Set Window)

type State = TVar (Display, UserFocus)

type UserFocus = Desktop

moveWindowSTM :: State -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM state win desktopA desktopB = do
  (disp, focus) <- readTVar state
  let winSetA = disp ! desktopA
      winSetB = disp ! desktopB
      newWinSetA = Set.delete win winSetA
      newWinSetB = Set.insert win winSetB
      newDisp =
        Map.insert desktopB newWinSetB $ Map.insert desktopA newWinSetA disp
  writeTVar state (newDisp, focus)

moveWindow :: State -> Window -> Desktop -> Desktop -> IO ()
moveWindow state win a b = atomically $ moveWindowSTM state win a b

swapWindows :: State
            -> Window -> Desktop
            -> Window -> Desktop
            -> IO ()
swapWindows state w a v b = atomically $ do
  moveWindowSTM state w a b
  moveWindowSTM state v b a

newState :: IO State
newState =
  let disp =
        Map.fromList
          [ (Desktop 1, Set.fromList [Window 1, Window 2])
          , (Desktop 2, Set.fromList [Window 3, Window 4])
          ]
  in newTVarIO (disp, Desktop 1)

showState :: State -> IO ()
showState = atomically . readTVar >=> print

testSwap :: IO ()
testSwap = do
  state <- newState
  putStrLn "starting state:"
  showState state
  swapWindows state (Window 1) (Desktop 1) (Window 3) (Desktop 2)
  putStrLn "ending state:"
  showState state

render :: Set Window -> IO ()
render = print


getWindows :: State -> STM (Set Window)
getWindows state = do
  (disp, focus) <- readTVar state
  pure $ disp ! focus

renderThread :: State -> IO ()
renderThread state = do
  wins <- atomically $ getWindows state
  loop wins
 where
  loop wins = do
    render wins
    next <- atomically $ do
               wins' <- getWindows state
               if (wins == wins')
                   then retry
                   else return wins'
    loop next

switchFocus :: State -> UserFocus -> IO ()
switchFocus state newFocus =
  atomically $ do
    (disp, _) <- readTVar state
    writeTVar state (disp, newFocus)

test :: IO ()
test = do
  state <- newState
  threadId <- forkIO $ renderThread state
  threadDelay (3 * 1000 * 1000)
  swapWindows state (Window 1) (Desktop 1) (Window 3) (Desktop 2)
  threadDelay (5 * 1000 * 1000)
  switchFocus state (Desktop 2)
  threadDelay (5 * 1000 * 1000)
  moveWindow state (Window 2) (Desktop 1) (Desktop 2)
  threadDelay (5 * 1000 * 1000)
  moveWindow state (Window 4) (Desktop 2) (Desktop 1)
  threadDelay (1 * 1000 * 1000)
  killThread threadId
