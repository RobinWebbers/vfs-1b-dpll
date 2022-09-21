module List
  ( parents
  , ggParents
  , ggParents'
  , Sheep (..)
  ) where

-- The List monad:
-----------------------------------------------------------
-- The list monad allows to compose computations that
-- may return any number of results.
--
-- Like maybe, the chain of computations essentially halts
-- when an empty list is returned.
-----------------------------------------------------------

import Data.Maybe
import Control.Monad

data Sheep = Sheep
  { mother :: Maybe Sheep
  , father :: Maybe Sheep
  }
  deriving (Show, Eq)

-- Returns the parents of a sheep in a list
--
-- e.g. parents (Sheep Nothing Nothing) = []
--      parents (Sheep (Just (Sheep Nothing Nothing)) Nothing) = [Sheep Nothing Nothing]
parents :: Sheep -> [Sheep]
parents = undefined

-- Solve without monadic functions (no >>=, >=>, etc)
--       ^^^^^^^
-- Returns all the great grandparents (i.e. parents applied 3 times)
ggParents :: Sheep -> [Sheep]
ggParents = undefined

-- Solve with monadic functions
--       ^^^^
-- Returns all the great grandparents (i.e. parents applied 3 times)
ggParents' :: Sheep -> [Sheep]
ggParents' = undefined
