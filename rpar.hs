{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
import Control.Monad
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

-- <<main
main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4] !! (read n - 1)
  t0 <- getCurrentTime
  -- Replacing 'evaluate' with 'return' causes all of the tests to return immediately
  -- (since nothing is being evaluated).
  -- r <- return (runEval test)
  -- Adding 'force' after 'evaluate' causes all of the tests to execute immediately,
  -- and it takes the entire time for any results to be returned for ALL tests.
  -- r <- evaluate $ force (runEval test)
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
-- >>

-- <<test1
test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)
-- >>

-- <<test2
test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)
-- >>

-- <<test3
test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  rseq x
  return (x,y)
-- >>

-- <<test4
test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  rseq x
  rseq y
  return (x,y)
-- >>

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-------------------------------------------------------------

myMaybeUndefinedRpar :: Maybe Int
myMaybeUndefinedRpar = runEval $ rparWith rdeepseq undefined >>= rpar . Just

myMaybeUndefinedMyI :: Maybe Int
myMaybeUndefinedMyI = runMyI $ MyI undefined >>= MyI . Just

data MyI a = MyI a deriving (Functor, Show)

runMyI :: MyI a -> a
runMyI (MyI a) = a

instance Applicative MyI where { pure = MyI ; (<*>) = ap }
instance Monad MyI where { MyI a >>= f = f a }

tryWithUndefined = case myMaybeUndefinedRpar of Just _ -> print "hello"
-- tryWithUndefined = case myMaybeUndefinedMyI of Just _ -> print "hello"

myUndefinedRpar' :: Int
myUndefinedRpar' = runEval $ rparWith rdeepseq undefined

myUndefinedParPair :: (Int, Int)
myUndefinedParPair = runEval $ (,) <$> rseq undefined <*> rpar 3

myUndefinedMyIPair :: (Int, Int)
myUndefinedMyIPair = runMyI $ (,) <$> seq undefined (MyI undefined) <*> MyI undefined

tryWithUndefinedPair = case myUndefinedParPair of (_, _) -> print "hello"
