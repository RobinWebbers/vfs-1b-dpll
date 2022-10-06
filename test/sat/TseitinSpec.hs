module TseitinSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Rubric

import Data.List (find)
import qualified Tseitin
import qualified Prop as P
import CNF

import Parser (parse)

tests :: Rubric
tests = do
  criterion "tseitin" 1 . passOrFail $ do
    it "has correct lit case" $ do
      let And ands = Tseitin.equisat $ P.Lit 0
      length ands @?= 1
      ands @?= [Or [Lit 0]]
    it "has correct neg case" $ do
      let And ands = Tseitin.equisat $ P.Neg (P.Lit 0)
      length ands @?= 3

      find (== Or [Lit 1]) ands        @?= Just (Or [Lit 1])
      find (== Or [Lit 1, Lit 0]) ands @?= Just (Or [Lit 1, Lit 0])
      find (== Or [Neg 0, Neg 1]) ands @?= Just (Or [Neg 0, Neg 1])
    it "has correct conjunct case" $ do
      let And ands = Tseitin.equisat $ (P.Lit 0 P.:&: P.Lit 1)
      length ands @?= 4

      find (== Or [Lit 2]) ands        @?= Just (Or [Lit 2])
      find (== Or [Lit 2, Neg 1, Neg 0]) ands @?= Just (Or [Lit 2, Neg 1, Neg 0])
      find (== Or [Lit 1, Neg 2]) ands @?= Just (Or [Lit 1, Neg 2])
      find (== Or [Lit 0, Neg 2]) ands @?= Just (Or [Lit 0, Neg 2])
    it "has correct disjunct case" $ do
      let And ands = Tseitin.equisat $ (P.Lit 0 P.:|: P.Lit 1)
      length ands @?= 4

      find (== Or [Lit 2]) ands        @?= Just (Or [Lit 2])
      find (== Or [Lit 2, Neg 1]) ands @?= Just (Or [Lit 2, Neg 1])
      find (== Or [Lit 2, Neg 0]) ands @?= Just (Or [Lit 2, Neg 0])
      find (== Or [Lit 1, Lit 0, Neg 2]) ands @?= Just (Or [Lit 1, Lit 0, Neg 2])
    it "recurses correctly" $ do
      let equisat str = let And ands = Tseitin.equisat . parse $ str
                         in ands
      length (equisat "(a & b) | (c & d) | (e & f)") @?= 16
      length (equisat "(--a & -----b) | (c & d) | (e & f)") @?= 30
      length (equisat "-((a & b) | -((c | d) & (e & f)))") @?= 20
      length (equisat "-(-((c | d) & (e & f)) | (a & b))") @?= 20

