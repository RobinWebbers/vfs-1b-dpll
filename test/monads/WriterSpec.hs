module WriterSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Rubric

import Control.Monad.Writer

import qualified Writer

type Compute = (String -> Int -> (Int, String))

testCompute, testComp1, testComp2, testComp3 :: Compute -> Rubric
testCompute compute = passOrFail $ do 
  it "correctly computes and logs" $ do
    compute ".test"    5 @?= (   22, ".test.compute.comp1.comp2.comp3")
    compute ""       180 @?= (32407, ".compute.comp1.comp2.comp3")
    compute ".bogus"  99 @?= ( 9798, ".bogus.compute.comp1.comp2.comp3")

testComp1 comp1 = passOrFail $ do
  it "correctly computes and logs" $ do
    comp1 ".test"    5 @?= (   24, ".test.comp1")
    comp1 ""       180 @?= (32399, ".comp1")
    comp1 ".bogus"  99 @?= ( 9800, ".bogus.comp1")

testComp2 comp2 = passOrFail $ do
  it "correctly computes and logs" $ do
    comp2 ".test"    5 @?= (  8, ".test.comp2")
    comp2 ""       180 @?= (183, ".comp2")
    comp2 ".bogus"  99 @?= (102, ".bogus.comp2")

testComp3 comp3 = passOrFail $ do
  it "correctly computes and logs" $ do
    comp3 ".test"    5 @?= (  0, ".test.comp3")
    comp3 ""       180 @?= (185, ".comp3")
    comp3 ".bogus"  99 @?= ( 94, ".bogus.comp3")

tests :: Rubric
tests = distribute $ do
  let unwrap wr str i = runWriter (tell str >> wr i)
  distributed "compute"  $ testCompute Writer.compute
  distributed "comp1"    $ testComp1 Writer.comp1
  distributed "comp2"    $ testComp2 Writer.comp2
  distributed "comp3"    $ testComp3 Writer.comp3
  distributed "compute'" $ testCompute $ unwrap Writer.compute'
  distributed "comp1'"   $ testComp1 $ unwrap Writer.comp1'
  distributed "comp2'"   $ testComp2 $ unwrap Writer.comp2'
  distributed "comp3'"   $ testComp3 $ unwrap Writer.comp3'
