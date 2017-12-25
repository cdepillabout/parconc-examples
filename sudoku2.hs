import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import Data.Maybe
import Sudoku
import System.Environment

pppMap :: (a -> b) -> [a] -> Eval [b]
pppMap f [] = pure []
pppMap f (a:as) = do
  b <- rpar (f a)
  bs <- pppMap f as
  pure (b:bs)

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
        -- runEval $ do
        --   -- Since this only evaluates to WHNF, it doesn't cause any of the work
        --   -- to take place in parallel.  That's why we need force.
        --   as' <- rpar $ map solve as
        --   bs' <- rpar $ map solve bs
        --   as'' <- rseq as'
        --   bs'' <- rseq bs'
        --   pure (as'' ++ bs'')
        -- runEval $ parTraversable rseq (map solve puzzles)
        -- map solve puzzles `using` parTraversable rseq
        -- parMap rseq solve puzzles
        -- parMap rdeepseq solve puzzles
        runEval $ pppMap solve puzzles
  -- print (length (filter isJust solutions))
  -- This doesn't work because it is not evaluated to normal form.
  -- _ <- evaluate solutions
  _ <- evaluate $ force solutions
  pure ()
