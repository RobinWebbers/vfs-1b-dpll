module Maybe
  ( half
  , oneEighth
  , oneEighth'
  , oneEighth''
  ) where

import Control.Monad

-- The Maybe monad:
-----------------------------------------------------------
-- As discussed in class, the Maybe monad allows to compose
-- computations that may fail. 
--
-- To elaborate, the bind (>>=) applies functions until
-- one of the applications fails, at which point the
-- computation chain evaluates to Nothing
--
-- If at a loss for what to do, check out the work-group
-- lecture slides!
-----------------------------------------------------------

-- Returns the integer divided by two on even numbers.
-- On odd numbers, returns Nothing.
--
-- e.g. half 4  = Just 2
--      half 15 = Nothing
half :: Int -> Maybe Int
half = undefined

-- Returns one eighth of the number passed.
-- Compute this without monads (e.g. by pattern matching)
--              ^^^^^^^^^^^^^^
-- You have to compute this by continuously applying `half`, 
-- no points otherwise.
--
-- e.g. oneEighth 32 = Just 4
-- e.g. oneEighth 30 = Nothing
oneEighth :: Int -> Maybe Int
oneEighth = undefined

-- Returns one eighth of the number passed.
-- Compute this using monadic functions (>>= and `return`)
--                    ^^^^^^^^^^^^^^^^^
-- You have to compute this by continuously applying `half`, 
-- no points otherwise.
--
-- e.g. oneEighth' 32 = Just 4
-- e.g. oneEighth' 30 = Nothing
--
-- You could also do this with the (>=>) operator, which
-- is like the (.) operator but for monads :)
oneEighth' :: Int -> Maybe Int
oneEighth' = undefined

-- Returns one eighth of the number passed.
-- Compute this using `do` notation (i.e. only implicitly use >>=)
--                    ^^^^^^^^^^^^^
-- You have to compute this by continuously applying `half`, 
-- no points otherwise.
--
-- e.g. oneEighth'' 32 = Just 4
-- e.g. oneEighth'' 30 = Nothing
oneEighth'' :: Int -> Maybe Int
oneEighth'' = undefined
