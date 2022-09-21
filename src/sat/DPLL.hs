module DPLL
  ( satisfiable
  , dpll
  , resolve
  , bcp
  , ple
  , neg
  ) where

import Data.Maybe
import Control.Applicative

import CNF

-- Negate a literal. Usefull for `resolve` and branches in `dpll`
neg :: Lit a -> Lit a
neg (Lit x) = Neg x
neg (Neg x) = Lit x

-- Unit Resolution
--
-- Given a literal and a CNF, remove all occurences
-- of it in the CNF according to Unit Resolution.
-- This implementation can be useful in `bcp` as well as
-- when branching in `dpll`.
--
-- e.g. resolve (Lit p) (And [Or [Lit p], Or [q], Or [Neg p]] )
--   == And [Or [q], Or []]
--
-- Notice how an occurence of the literal resolves the whole Or
-- while occurence of its negation removes the literal.
resolve :: Eq a => Lit a -> CNF a -> CNF a
resolve = undefined

-- Boolean Constraint Propagation (BCP)
--
-- Remove all occurences of single variables according
-- to Boolean Constraint Propagation.
bcp :: Eq a => CNF a -> CNF a
bcp = undefined

-- Pure Literal Elimination (PLE)
--
-- Resolve variables if they only occur positively or
-- negatively (but not both), according to Pure Literal
-- Elimination
ple :: Eq a => CNF a -> CNF a
ple = id

-- The DPLL procedure. 
--
-- Finds whether a given CNF is satisfiable.
--
-- A CNF is satisfiable when all of its conjuncts
-- are resolved. In our case, when we have
-- And [] :: CNF a
--
-- A CNF is unsatisfiable when it contains an Or
-- that contains no literals. e.g.
-- And [Or [], ...] is unsat
--
-- Apart from that, to get you started you can implement
-- this procedure without doing BCP. This
-- means that we just naively pick any element
-- to branch on and do a depth-first search.
--
-- Once this works, you can incorporate BCP.
-- We have decided to leave out PLE for the assignment
-- this year to reduce workload.
--
-- We don't expect any heuristics to pick branches 
-- in this implementation. Thus, you may just pick
-- any literal to branch on.
--
-- As a note on the return type. You may return `pure []`
-- when it is satifiable and `empty` otherwise. This allows
-- you to compose branches really nicely via the (<|>) operator!
dpll :: (Alternative f, Eq a) => CNF a -> f ()
dpll = undefined

-- Check a given CNF for satisfiability. Returning
-- True of the cnf is satisfiable.
satisfiable :: Eq a => CNF a -> Bool
satisfiable = isJust . dpll
