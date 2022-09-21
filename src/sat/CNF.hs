module CNF
  ( rigid
  , CNF (..)
  , Or (..)
  , Lit (..)
  ) where

import Prop hiding (Neg, Lit)
import qualified Prop as P

-- In a CNF, we either have a literal
-- or a negation of it (no double negation, etc.)
data Lit a = Lit a | Neg a
  deriving (Show, Eq)
-- All elements in the CNF type are Disjunct
-- to each other. 
--
-- e.g. [p, q, r] == p | q | r
--      []        == False
newtype Or a = Or [Lit a]
  deriving (Show, Eq)
-- All elements in the CNF type are Conjunct
-- to each other.
--
-- e.g. [p, q, r] == p & q & r
--      []        == True
newtype CNF a = And [Or a]
  deriving (Show, Eq)

-- Notice how the list append is in reverse,
-- which saves us from traversing the whole
-- list when appending in the Writer monad!
-- The same goes for CNF
instance Semigroup (Or a) where
  (Or xs) <> (Or ys) = Or $ ys ++ xs
instance Monoid (Or a) where
  mempty = Or []

instance Semigroup (CNF a) where
  (And xs) <> (And ys) = And $ ys ++ xs
instance Monoid (CNF a) where
  mempty = And []

-- Write the propositional value as a rigid CNF. 
-- This way, the type system ensures that there
-- is no way for the Prop expression to not be
-- in CNF.
rigid :: Prop a -> CNF a
rigid p = case ands . cnf $ p of
  Just p' -> p'
  Nothing -> error "Unreachable: {v:Prop a | cnf v} -> CNF a"

-- Accumulate all the ands, nesting to ors
ands :: Prop a -> Maybe (CNF a)
ands (p :&: q) = do
  p' <- ands p
  q' <- ands q
  return $ p' <> q'
ands p = ors p >>= return . And . return

-- Accumulate all the ors, nesting to literals
ors :: Prop a -> Maybe (Or a)
ors (p :|: q) = do
  p' <- ors p
  q' <- ors q
  return $ p' <> q'
ors p = lit p >>= return . Or . return

-- Get a literal or fail
lit :: Prop a -> Maybe (Lit a)
lit (P.Neg (P.Lit x)) = return . Neg $ x
lit        (P.Lit x)  = return . Lit $ x
lit _                 = Nothing
