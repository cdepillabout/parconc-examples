
{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}
-- -Wall 

-- A module for stream processing built on top of Control.Monad.Par

-- (In the future may want to look into the stream interface used by
--  the stream fusion framework.)

module Stream2
 ( 
   IList(..), streamFromList, streamMap, streamFold {-, streamFilter -}
 ) where

import Control.Monad.Par.Scheds.Trace as P
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Types

-- <<IList
data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)
-- >>

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

-- -----------------------------------------------------------------------------
-- Stream operators

-- <<streamFromList
streamFromList :: forall a. NFData a => Int -> [a] -> Par (IVar (IList a))
streamFromList n xs = do
  var <- new                            -- <1>
  fork $ loop 0 xs var                    -- <2>
  return var                            -- <3>
 where
  loop :: Int -> [a] -> IVar (IList a) -> Par ()
  loop _ [] var = put var Nil             -- <4>
  loop i (x:xs) var
    | i == n = do
        tail <- new
        put var $ Fork (loop 0 xs tail) (Cons x tail)
    | otherwise = do                  -- <5>
        tail <- new                         -- <6>
        put var (Cons x tail)               -- <7>
        loop (succ i) xs tail                        -- <8>
-- >>

-- <<streamMap
streamMap
  :: forall a b. NFData b => Int -> (a -> b) -> IVar (IList a) -> Par (IVar (IList b))
streamMap n fn instrm = do
  outstrm <- new
  fork $ loop 0 instrm outstrm
  return outstrm
 where
  loop :: Int -> IVar (IList a) -> IVar (IList b) -> Par ()
  loop i instrm outstrm
    | i == n = do
        xxx <- new
        ilst <- get instrm
        put outstrm $ Fork (loop 0 instrm xxx) ilst
    | otherwise = do
        ilst <- get instrm
        f ilst
        where
          f :: IList a -> Par ()
          f ilst =
            case ilst of
              Nil -> put outstrm Nil
              Cons h t -> do
                newtl <- new
                put outstrm (Cons (fn h) newtl)
                loop (succ i) t newtl
              Fork p inner -> do
                fork p
                f inner
-- >>


-- | Reduce a stream to a single value.  This function will not return
--   until it reaches the end-of-stream.
-- <<streamFold
streamFold :: forall a b. (a -> b -> a) -> a -> IVar (IList b) -> Par a
streamFold fn !acc instrm = do
  ilst <- get instrm
  f ilst
  where
    f :: IList b -> Par a
    f ilst =
      case ilst of
        Nil -> return acc
        Cons h t -> streamFold fn (fn acc h) t
        Fork p innerIList -> do
          fork p
          f innerIList
-- >>

-- streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
-- streamFilter p instr = do
--     outstr <- new
--     fork $ loop instr outstr
--     return outstr
--   where
--     loop instr outstr = do
--       v <- get instr
--       case v of
--         Nil -> put outstr Nil
--         Cons x instr'
--           | p x -> do
--              tail <- new
--              put_ outstr (Cons x tail)
--              loop instr' tail
--           | otherwise -> do
--              loop instr' outstr
