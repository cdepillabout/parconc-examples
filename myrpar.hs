{-# LANGUAGE MagicHash #-}

module MyPar where

import Control.DeepSeq
import Control.Monad
import GHC.Exts (lazy, par#)

data Eval a = Done a

runEval :: Eval a -> a
runEval (Done x) = x

instance Functor Eval where
  fmap f (Done x) = Done (f x)

instance Applicative Eval where
  pure = Done
  (<*>) = ap

instance Monad Eval where
  return = pure
  Done x >>= k = lazy (k x)   -- Note: pattern 'Done x' makes '>>=' strict

-- type Strategy a = a -> Eval a

using :: a -> (a -> Eval a) -> a
-- using x strat = runEval (strat x)
using x strat = case strat x of Done x -> x

withStrategy :: (a -> Eval a) -> a -> a
withStrategy strat x = case strat x of Done x -> x

dot :: (a -> Eval a) -> (a -> Eval a) -> a -> Eval a
dot strat2 strat1 = strat2 . (\(Done x) -> x) . strat1

r0 :: a -> Eval a
r0 = Done

rseq :: a -> Eval a
rseq x = seq x (Done x)

rdeepseq :: NFData a => a -> Eval a
-- rdeepseq x = do
--   rseq (rnf x)
--   return x
-- rdeepseq x = do
--   let z = rnf x in seq z (Done z)
--   return x
-- rdeepseq x = lazy ((\() -> Done x) (let z = rnf x in seq z z))
rdeepseq x = lazy $ rnf x `seq` Done x

rpar :: a -> Eval a
rpar  x = case par# x of _ -> Done x

rparWith :: (a -> Eval a) -> a -> Eval a
-- rparWith s a = do
--   l <- rpar (s a)
--   return (case l of Done x -> x)
-- rparWith s a = rpar (s a) >>= \l -> return (case l of Done x -> x)
-- rparWith s a = lazy $ (\(Done l) -> return (case l of Done x -> x)) (rpar (s a))
-- rparWith s a = lazy $ (\(Done l) -> Done (case l of Done x -> x)) (rpar (s a))
-- rparWith s a = lazy $ (\(Done (Done x)) -> Done x) (rpar (s a))
-- rparWith s a =
--   lazy $ (\(Done (Done x)) -> Done x) (let z = s a in case par# z of _ -> Done z)
rparWith s a = lazy $ let z = s a in case par# z of _ -> z


evalTraversable :: Traversable t => (a -> Eval a) -> t a -> Eval (t a)
evalTraversable = traverse


-- TODO: Figure out this function.
parTraversable :: Traversable t => (a -> Eval a) -> t a -> Eval (t a)
-- parTraversable strat = evalTraversable (rparWith strat)
parTraversable s = traverse (\a -> lazy $ let z = s a in case par# z of _ -> z)
