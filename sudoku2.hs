import Sudoku
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f
  let puzzles = lines file
      (as,bs) = splitAt (length puzzles `div` 2) puzzles
      solutions =
        -- runEval $ do
        --   as' <- rpar (force $ map solve as)
        --   bs' <- rpar (force $ map solve bs)
        --   rseq as'
        --   rseq bs'
        --   pure (as' ++ bs')
        -- runEval $ do
        --   as' <- rpar (force $ map solve as)
        --   bs' <- rpar (force $ map solve bs)
        --   as'' <- rseq as'
        --   bs'' <- rseq bs'
        --   pure (as'' ++ bs'')
        -- runEval $ do
        --   as' <- rparWith rdeepseq $ map solve as
        --   bs' <- rparWith rdeepseq $ map solve bs
        --   as'' <- rseq as'
        --   bs'' <- rseq bs'
        --   pure (as'' ++ bs'')
        runEval $ do
          -- Since this only evaluates to WHNF, it doesn't cause any of the work
          -- to take place in parallel.  That's why we need force.
          as' <- rpar $ map solve as
          bs' <- rpar $ map solve bs
          as'' <- rseq as'
          bs'' <- rseq bs'
          pure (as'' ++ bs'')
  print (length (filter isJust solutions))
