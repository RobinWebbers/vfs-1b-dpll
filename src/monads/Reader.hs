module Reader
  ( compute
  , compute'
  , compute''
  , Config (..)
  ) where

import Control.Monad.Reader

-- The Reader monad:
-----------------------------------------------------------
-- Often times, we wish to have access to some auxilliary
-- environment or global state when doing computation. However,
-- as Haskell has no global state we need a different way to
-- express this.
--
-- `Reader r a` is a monad that encapsulates a computation 
-- which has some readable environment `r` and computes `a`.
-- To elaborate, `Reader r` is the `m` in monad `m a`.
--
-- An example usage would be a command line utility that
-- has some flags that alter its behaviour. The flags
-- may then be stored in the Reader monad
--
-- Consider the following function
-- fn :: Reader String Int
-- fn = ...
--
-- Here, `fn` is essentially a computation that has an
-- readable environment String and returns an Int.
--
-- To read the environment, one can use `ask` and `reader`. e.g.
-- fn :: Reader String Int
-- fn = do
--   str <- ask
--   ...
--
-- Essentially, `ask` will put the environment on the output
-- of the Reader, so:
-- ask fn :: Reader String String
--
-- Then we can monadically bind the environemnt to actually
-- read it:
-- fn' = ask fn >>= \str -> ...
--
-- We of course recommend the do-notation over this, but
-- do make sure that you thoroughly understand what actually
-- happens!
--
-- To run the compuation use `runReader`
-- eval = runReader fn "test"
-----------------------------------------------------------

-- A data structure that contains some configurations for 
-- the computation.
data Config = Cfg
  { add :: Int
  , sub :: Int
  }

-- Compute without the Reader monad,
-- The passed int + the add field of cfg - the sub field of cfg
--
-- e.g. compute 5 (Cfg 4 6) = 3
compute :: Int -> Config -> Int
compute = undefined

-- Compute using the Reader monad and binds (>>=), (no do-notation)
--                                    ^^^^^^^^^^^
-- The passed int + the add field of cfg - the sub field of cfg
--
-- e.g. runReader (compute' 5) $ Cfg 4 6 = 3
compute' :: Int -> Reader Config Int
compute' = undefined

-- Compute using the Reader monad and do-notation,
--                                    ^^^^^^^^^^^
-- The passed int + the add field of cfg - the sub field of cfg
--
-- e.g. runReader (compute' 5) $ Cfg 4 6 = 3
compute'' :: Int -> Reader Config Int
compute'' = undefined
