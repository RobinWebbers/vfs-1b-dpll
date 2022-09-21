module WriterSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Control.Monad.Writer

import qualified Writer

type Compute = (String -> Int -> (Int, String))

testCompute, testComp1, testComp2, testComp3 :: Compute -> Spec
testCompute compute = 
  it "correctly computes and logs" $ do
    compute ".test"    5 @?= (   22, ".test.compute.comp1.comp2.comp3")
    compute ""       180 @?= (32407, ".compute.comp1.comp2.comp3")
    compute ".bogus"  99 @?= ( 9798, ".bogus.compute.comp1.comp2.comp3")

testComp1 comp1 =
  it "correctly computes and logs" $ do
    comp1 ".test"    5 @?= (   24, ".test.comp1")
    comp1 ""       180 @?= (32399, ".comp1")
    comp1 ".bogus"  99 @?= ( 9800, ".bogus.comp1")

testComp2 comp2 =
  it "correctly computes and logs" $ do
    comp2 ".test"    5 @?= (  8, ".test.comp2")
    comp2 ""       180 @?= (183, ".comp2")
    comp2 ".bogus"  99 @?= (102, ".bogus.comp2")

testComp3 comp3 =
  it "correctly computes and logs" $ do
    comp3 ".test"    5 @?= (  0, ".test.comp3")
    comp3 ""       180 @?= (185, ".comp3")
    comp3 ".bogus"  99 @?= ( 94, ".bogus.comp3")

tests :: Spec
tests = do
  let unwrap wr str i = runWriter (tell str >> wr i)
  describe "compute"  $ testCompute Writer.compute
  describe "comp1"    $ testComp1 Writer.comp1
  describe "comp2"    $ testComp2 Writer.comp2
  describe "comp3"    $ testComp3 Writer.comp3
  describe "compute'" $ testCompute $ unwrap Writer.compute'
  describe "comp1'"   $ testComp1 $ unwrap Writer.comp1'
  describe "comp2'"   $ testComp2 $ unwrap Writer.comp2'
  describe "comp3'"   $ testComp3 $ unwrap Writer.comp3'
