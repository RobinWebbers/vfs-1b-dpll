module Rubric
  ( Criteria
  , Criterion (..)
  , grade
  , sanity

  , RubricM
  , runRubricM
  , evalRubricM

  , Rubric
  , criterion
  , distributed
  , passes
  , distribute
  , passOrFail
  ) where

import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Runner hiding (Path)
import Test.HUnit hiding (Path)

import Data.List (foldl', isPrefixOf)
import Debug.Trace

import Control.Monad.Writer

-- The list of criteria in a rubric.
-- The final weigth of the criteria
-- should add up to 1.0 (or just be
-- distribute for all instances)
type Criteria = [Criterion]

-- A criterion in a rubric. Contains
-- nested rubrics in a tree to place
-- weights on sub-criteria
data Criterion = Criterion
  { name   :: String
  , weight :: Float
  -- Empty rubric means pass or fail
  , nodes  :: Criteria
  }
  deriving (Show, Eq)

-- A path to a test
type Path = [String]

-- Award a grade for the results given a rubric
grade :: Criteria -> SpecResult -> Float
grade criteria result
  | sanity criteria = check (expand criteria) (specResultItems result)
  | otherwise       = error "rubric did not pass the sanity test..."
  where
    -- Check all paths of the rubric against the results
    check :: [(Float, Path)] -> [ResultItem] -> Float
    check rs is = foldl' (\g r -> g + award r is) 0 rs

    -- Award points if the result items that match all passed
    award :: (Float, Path) -> [ResultItem] -> Float
    award (g, p) is = do
      let matches = filter (subpath p) is
      -- No points if there is a failure.
      -- A test case wasn't run if there were no matches,
      -- so we don't award points in that case.
      if null matches || any resultItemIsFailure matches
        then 0
        else g

    -- Check if a Path path matches the item path
    subpath :: Path -> ResultItem -> Bool
    subpath p q = do
      let (q', n) = resultItemPath q
      isPrefixOf p (q' ++ [n])

-- Expand criteria to a list of all tests paths and their weights
expand :: Criteria -> [(Float, Path)]
expand = expand' (1.0, [])
  where
    expand' :: (Float, Path) -> Criteria -> [(Float, Path)]
    expand' tup []        = [tup]
    expand' (g, p) (c:[]) = recurse g p c
    expand' (g, p) (c:cs) = recurse g p c ++ expand' (g, p) cs

    -- calculate the weigth, append name and recursively expand paths
    recurse g p c = expand' (g * weight c, p ++ [name c]) (nodes c)

-- Recursively check whether the weights are sane (e.g. accumulate to 1)
sanity :: Criteria -> Bool
sanity [] = True
sanity rs = cmp 1 weights && all (sanity . nodes) rs
  where
    -- Weights of the current rubric node
    weights = foldl' (\acc r -> acc + weight r) 0 rs

    -- Adjust for floating point errors
    cmp x y = abs (x-y) < 0.0001

-- We cannot stack monads on SpecM, because we cannot
-- call runSpecM on a SpecM Rubric (only on SpecM ()
-- which means there is no way to get the rubric out
-- of the WriterT monad when SpecM is stacked into
-- it...)
--
-- Hence, we have this cursed Monad instead.
data RubricM s a = RubricM (Writer Criteria a) (SpecWith s)

type Rubric = RubricM () ()

instance Functor (RubricM s) where
  fmap f (RubricM w s) = RubricM (f <$> w) s

instance Applicative (RubricM a) where
  pure x = RubricM (pure x) (pure ())
  (<*>) = undefined

instance Monad (RubricM a) where
  RubricM r s >>= f = RubricM r'' s''
    where
      ~(a, w ) = runWriter r
      RubricM r' s' = f a
      ~(b, w') = runWriter $ r'
   
      r'' = writer (b, w <> w')
      s''  = s >> s'

-- Run the rubric monad
runRubricM :: RubricM s a -> ((a, Criteria), SpecWith s)
runRubricM (RubricM c s) = (runWriter c, s)

-- Run the rubric monad, but get only the criteria
evalRubricM :: RubricM s a -> (Criteria, SpecWith s)
evalRubricM (RubricM c s) = (execWriter c, s)

-- Set up a criterion in the rubric. Like HSpec `describe` but with points.
criterion :: HasCallStack => String -> Float -> RubricM s a -> RubricM s a
criterion n w (RubricM c s) = RubricM c' (describe n s)
  where
    c' = writer (a, [Criterion n w tree])
    (a, tree) = runWriter c

-- A shorthand for a criterion where the points will be
-- distributed later on by `distribute`. (this will award
-- NaN points)
distributed :: HasCallStack => String -> RubricM s a -> RubricM s a
distributed = flip criterion (0/0)

-- A test that awards points. Like HSpec `it` but with points.
passes :: (HasCallStack, Example s) => String -> Float -> s -> RubricM (Arg s) ()
passes n w s = RubricM (writer ((), [crit])) (it n s)
  where
    crit = Criterion n w []

-- Distribute points among the current level of items.
distribute :: HasCallStack => RubricM s a -> RubricM s a
distribute (RubricM r s) = RubricM (writer (a, criteria')) s
  where
    (a, criteria) = runWriter r
    w = 1 / (fromIntegral . length) criteria
    criteria' = map (\r' -> r' { weight = w }) criteria

-- A combinator that transforms an HSpec SpecWith into a pass-or-fail rubric.
-- So this awards full points iff all tests passed.
passOrFail :: HasCallStack => SpecWith s -> RubricM s ()
passOrFail s = RubricM (writer ((), [])) s
