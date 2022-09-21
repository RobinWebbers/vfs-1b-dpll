module DPLLSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit

import CNF

import qualified DPLL
import Parser (parse)
import qualified Tseitin

sat :: String -> Bool
sat = DPLL.satisfiable . Tseitin.equisat . parse

tests :: Spec
tests = do
  describe "resolve" $ do
    it "removes Or if it contained the literal" $ do
      DPLL.resolve (Lit 0) (And []) @?= And [] 
      DPLL.resolve (Lit 0) (And [Or [Lit 0]]) @?= And [] 
      DPLL.resolve (Lit 0) (And [Or [], Or [Lit 1, Lit 0, Lit 2], Or [Lit 2]]) @?= And [Or [], Or [Lit 2]]
    it "removes literal from Or if negation was contained" $ do
      DPLL.resolve (Lit 0) (And [Or [Neg 0]]) @?= And [Or []] 
      DPLL.resolve (Neg 2) (And [Or [], Or [Lit 1, Lit 0, Lit 2], Or [Lit 2]]) @?= And [Or [], Or [Lit 1, Lit 0], Or []]
  describe "dpll" $ do
    it "computes satisfiability" $ do
      sat "a & b & c & d & e & f & g & (-a | -b)" @?= False
      sat "a & -b & c & d & e & f & g & (-a | -b)" @?= True
      sat "(a | b | c) & (-a | -b | -c)" @?= True
      sat "(a | b | c) & (-a & (-b | -(a & c)) | -c)" @?= True
      sat "(a | b | c | d | e) & -a & -b & -c & -d & -e" @?= False
      sat "(x | y | z) & (x | y | -z) & (x | -y | z) & (x | -y | -z) & (-x | y | z) & (-x | y | -z) & (-x | -y | z) & (-x | -y | -z)" @?= False
  describe "bcp" $ do
    it "resolves all occurences of single variables" $ do
      DPLL.bcp (And [Or [Neg 0], Or [Lit 0, Lit 1, Lit 2]]) @?= And [Or [Lit 1, Lit 2]]
      DPLL.bcp (And [Or [Lit 0, Lit 1, Lit 2], Or [Lit 0]]) @?= And []
      DPLL.bcp (And [Or [Neg 0, Lit 1, Lit 2, Neg 3], Or [Lit 0], Or [Lit 3]]) @?= And [Or [Lit 1, Lit 2]]
