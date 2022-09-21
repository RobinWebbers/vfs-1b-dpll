module PropSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit

import qualified Prop
import Prop (Prop (..), (-->), (<->))

import Parser (parse)

isCnf :: Prop a -> Bool
isCnf (p :&: q) = isCnf p && isCnf q
isCnf p         = isOr p

isOr :: Prop a -> Bool
isOr (p :|: q) = isOr p && isOr q
isOr p         = isLit p

isLit :: Prop a -> Bool
isLit (Lit _)       = True
isLit (Neg (Lit _)) = True
isLit _             = False

tests :: Spec
tests = do
  describe "-->" $ do
    it "is defined in terms of Neg and :|:" $ do
      Lit 0 --> Lit 1 @?= Neg (Lit 0) :|: Lit 1
      let p = Lit 0 :&: Lit 1
      let q = Lit 1 :|: Lit 2
      p --> q @?= Neg p :|: q
  describe "<->" $ do
    it "is defined in terms of --> and :&:" $ do
      Lit 0 <-> Lit 1 @?= (Neg (Lit 0) :|: Lit 1) :&: (Neg (Lit 1) :|: Lit 0)
  describe "distribute" $ do
    it "correctly computes the base case (no :&: in operands)" $ do
      Prop.distribute (Lit 0) (Lit 1) @?= Lit 0 :|: Lit 1
      let lhs = parse "a | b | c | d"
      let rhs = parse "e | f | g | h | j"
      Prop.distribute lhs rhs @?= lhs :|: rhs
    it "correctly distributes singles" $ do
      let lit  = parse "a"
      let and' = parse "b & c"
      Prop.distribute lit and' @?= parse "(a | b) & (a | c)"
      Prop.distribute and' lit @?= parse "(b | a) & (c | a)"
    it "correctly recursively distributes" $ do
      let lhs = parse "a & b"
      isCnf (Prop.distribute lhs $ parse "c & d")      @?= True
      isCnf (Prop.distribute lhs $ parse "c & d & e")  @?= True

      let lhs' = parse "a & b & c"
      isCnf (Prop.distribute lhs' $ parse "c & d")     @?= True
      isCnf (Prop.distribute lhs' $ parse "c & d & e") @?= True
  describe "cnf" $ do
    it "keeps literals (and their negation)" $ do
      Prop.cnf (Lit 0)       @?= Lit 0
      Prop.cnf (Neg (Lit 0)) @?= Neg (Lit 0)
    it "removes double negatives" $ do
      Prop.cnf (Neg (Neg (Lit 0))) @?= Lit 0
    it "distributes disjuncts (and leaves conjuncts)" $ do
      let and' = parse "a & b & c"
      Prop.cnf (Lit 0 :&: Lit 1) @?= (Lit 0 :&: Lit 1)
      Prop.cnf (and' :&: and')   @?= (and' :&: and')

      Prop.cnf (Lit 0 :|: Lit 1) @?= (Lit 0 :|: Lit 1)
      isCnf (Prop.cnf $ and' :|: parse "c & d")     @?= True
      isCnf (Prop.cnf $ and' :|: parse "c & (d & e)") @?= True
    it "applies De-Morgans law" $ do
      Prop.cnf (parse "-(p & q)") @?= parse "-p | -q"
      Prop.cnf (parse "-(p | q)") @?= parse "-p & -q"
    it "recursively applies all cases" $ do
      isCnf (Prop.cnf $ parse "(a & b) | (c & d) | (e & f)") @?= True
      isCnf (Prop.cnf $ parse "(--a & -----b) | (c & d) | (e & f)") @?= True
      isCnf (Prop.cnf $ parse "-((a & b) | -((c | d) & (e & f)))") @?= True
      isCnf (Prop.cnf $ parse "-(-((c | d) & (e & f)) | (a & b))") @?= True

