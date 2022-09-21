{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Prop
  (Prop (..)
  , (-->)
  , (<->)
  , distribute
  , cnf
  , assocl
  ) where

-- Define operator precedence for binary
-- operators. :&: binds before :|:, similar
-- to how * binds before + in arithmetic.
infixl 7 :&:
infixl 6 :|:
infixl 5 -->
infixl 4 <->

-- Propositional logic format
-- We define the data constructors of the minimal
-- language where (and = :&:, or = :|:)
data Prop a
  = Lit a
  | Neg (Prop a)
  | Prop a :&: Prop a
  | Prop a :|: Prop a
  deriving (Eq, Functor, Foldable, Traversable)

-- Show with operator precedence. This reduces
-- parentheses on prints, improving readability
-- considerably!
instance Show a => Show (Prop a) where
  showsPrec prec (Lit x)   = showsPrec prec x
  showsPrec prec (Neg p)   = showParen (prec >= 8) $ showString "¬" . showsPrec 7 p 
  showsPrec prec (p :&: q) = showParen (prec >= 7) $ showsPrec 6 p . showString " ∧ " . showsPrec 7 q
  showsPrec prec (p :|: q) = showParen (prec >= 6) $ showsPrec 5 p . showString " ∨ " . showsPrec 6 q

-- Left associate a propositional expression.
-- Since :&: and :|: are commutative, meaning
-- is preserved. This is usefull for printing
-- to improve readability by reducing parentheses.
assocl :: Prop a -> Prop a
assocl (p :&: (q :&: r)) = assocl $ p :&: q :&: r
assocl (p :|: (q :|: r)) = assocl $ p :|: q :|: r
assocl (p :&: q)         = assocl p :&: assocl q
assocl (p :|: q)         = assocl p :|: assocl q
assocl (Neg p)           = Neg $ assocl p
assocl p                 = p

-- Implication
(-->) :: Prop a -> Prop a -> Prop a
(-->) = undefined

-- Bi-Implication
(<->) :: Prop a -> Prop a -> Prop a
(<->) = undefined

-- This function implements the distribution 
-- of disjunction over conjunction:
-- (p :|: (q :&: r)) <-> (p :|: q) :&: (p :|: r)
--
-- This function does a full distribution of
-- two props that were connected via :|:.
--
-- This also needs to handle the commative case:
-- ((p :&: q) :|: r) <-> (p :|: r) :&: (q :|: r)
--
-- As well as the case where p, q and r are not 
-- literals but contain their own conjunctions
-- (do this via recusrion of dist)
distribute :: Prop a -> Prop a -> Prop a
distribute = undefined

-- Recursively transform a propositional formula into
-- Conjunct Normal Form.
--
-- Important cases to keep in mind:
-- - Double negation should be eliminated
-- - Distribute disjunctions
-- - De Morgans Law
cnf :: Prop a -> Prop a
cnf = undefined
