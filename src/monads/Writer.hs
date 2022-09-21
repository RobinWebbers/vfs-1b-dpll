module Writer
  ( compute
  , comp1
  , comp2
  , comp3
  , compute'
  , comp1'
  , comp2'
  , comp3'
  ) where

import Control.Monad.Writer
import Prelude hiding (log)

-- The Writer monad:
-----------------------------------------------------------
-- Often times, we need to write some auxilliary data
-- that is separate from our main computation. However,
-- as functions in Haskell cannot have side-effects,
-- we need a different way to express this.
--
-- `Writer w a` is a monad that encapsulates a computation 
-- which writes additional data `w` and computes `a`.
-- To elaborate, `Write w` is the `m` in monad `m a`.
--
-- An example usage would be a web server that writes
-- logs of a computation.
--
-- Consider the following function
-- fn :: Writer String Int
-- fn = ...
--
-- Here, `fn` is essentially a computation that "writes"
-- some String and computes an Int.
--
-- One can attach additional information to the writer
-- via the `writer` or `tell` functions. e.g.
-- fn :: Writer String Int
-- fn = do
--   tell ".test"
--   return 4
--
-- We can of course compose and evaluate a writer
-- runWriter (fn >> fn) == (4, ".test.test")
--
-- As an aside, `fn >> fn` is equivalent to `fn >>= \_ -> fn`
-----------------------------------------------------------

-- This applies comp1, comp2 and comp3 in order.
-- The Int is the main computation, the String
-- contains logs of the function we enter.
-- 
-- This functions should append (++ or <>) the string 
-- ".compute" to the initial log and chain the logs
-- from here.
--
-- e.g. compute "test" 4 = (23, "test.compute.comp1.comp2.comp3")
compute :: String -> Int -> (Int, String)
compute = undefined

-- Takes a power of 2 over the passed Int and subtract 1
-- Appends the log to the string ".comp1"
--
-- e.g. comp1 ".compute" 4 = (15, ".compute.comp1")
comp1 :: String -> Int -> (Int, String)
comp1 = undefined

-- Adds 3 to the passed Int
-- Appends the log to the string ".comp2"
--
-- e.g. comp2 ".test" 6 = (9, ".test.comp2")
comp2 :: String -> Int -> (Int, String)
comp2 = undefined

-- If the passed Int is even, add 5 else subtract 5
-- Appends the log to the string ".comp3"
--
-- e.g. comp3 ".test" 10 = (15, ".test.comp3")
-- e.g. comp3 "" 9  = (4, ".comp3")
comp3 :: String -> Int -> (Int, String)
comp3 = undefined

-- First, writes ".compute" 
-- Then, computes comp1, comp2 and comp3 in order.
--
-- e.g. runWriter $ compute' 4 = (23, "test.compute.comp1.comp2.comp3")
compute' :: Int -> Writer String Int
compute' = undefined

-- First, writes ".comp1"
-- Then, take a power of 2 over the passed Int and subtract 1
--
-- e.g. runWriter $ comp1' 4 = (15, ".comp1")
comp1' :: Int -> Writer String Int
comp1' = undefined

-- First, writes ".comp2"
-- Then, add 3 to the passed Int
--
-- e.g. runWriter $ comp2' 6 = (9, ".comp2")
comp2' :: Int -> Writer String Int
comp2' = undefined

-- First, writes ".comp2"
-- Then, if the passed Int is even, add 5 else subtract 5
--
-- e.g. runWriter $ comp3' 10 = (15, ".comp3")
-- e.g. runWriter $ comp3'  9 = ( 4, ".comp3")
comp3' :: Int -> Writer String Int
comp3' = undefined