{-# LANGUAGE FlexibleContexts #-}

module Tseitin
  ( equisat
  , fresh
  , ID
  ) where

import Prelude hiding (id)

import Numeric.Natural
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import Prop
import CNF (CNF)
import qualified CNF as CNF

-- An identifier for variables for which we can
-- easily generate fresh variables.
type ID = Natural

-- Get an identifier for a given object.
-- Equivalent objects get the same ID.
rename :: (MonadState (ID, Map a ID) m, Ord a) => a -> m ID
rename e = do
  (id, m) <- get
  case Map.lookup e m of
    Just id' -> return id'
    Nothing -> do 
      let id' = id + 1
      let m' = Map.insert e id m
      put (id', m')
      return id

-- Intern the passed proposition. (i.e. rename
-- all literals with IDs) and return the mapping
-- of IDs to a
intern :: (MonadState ID m, Ord a) => Prop a -> m (Prop ID, Map ID a)
intern p = do
  id <- get
  let (p', (id', m)) = runState (mapM rename p) (id, Map.empty)
  put id'
  let inverse = Map.foldrWithKey (flip Map.insert) Map.empty m
  return (p', inverse)

-- Get a fresh propositional literal
fresh :: MonadState ID m => m (Prop ID)
fresh = do
  id <- get
  put $ id + 1
  return $ Lit id

-- Tseitins transformation:
-- Transforms a Propositional formula into an equisatisfiable
-- CNF without the exponential space blow-up that happens
-- with the normal CNF transformation.
--
-- NOTICE: Make sure you thoroughly understand Tseitins
-- transformation before trying to implement this!
--
-- Why do we return CNF instead of Prop?
-- - CNF is a rigid form of Prop, such that it can only ever
--   be a CNF. Prop may be a CNF, but this isn't necessarily
--   the case. To avoid checks whether a Prop is in CNF during
--   DPLL, we omit this by ensuring this correctness property
--   at compile-time!
-- 
-- What are the Monad typeclasses?
-- - This function returns a monad that implements both the
--   State and Writer interfaces. This means that we can use
--   `put`, `get` and `tell`.
--
-- What do we need these for?
-- - The state is used to define fresh Literals.
--   The `fresh` helper function will return a fresh literal.
-- - The writer is used to accumulate the final outcome of
--   this function. Use `tell` to pass it a portion of the
--   CNF. Use `CNF.rigid` to get some propositional value
--   as a rigid CNF. (This uses your implementation of cnf,
--   it may crash if your implementation is incorrect)
--
-- Then what is contained in the return value of m?
-- - It contains (Prop ID), which in this case should
--   always be the renamed propositional value
-- - We don't rename literals, so you may directly
--   return a literal when encountered.
tseitin :: (MonadState ID m, MonadWriter (CNF ID) m) 
        => Prop ID -> m (Prop ID)
tseitin = undefined

-- Get an equisatisfiable CNF by tseitins transformation.
equisat :: Ord a => Prop a -> CNF ID
equisat p = flip evalState 0 $ do
  (q, _) <- intern p 
  (expr, ands) <- runWriterT $ tseitin q
  return $ ands <> CNF.rigid expr
