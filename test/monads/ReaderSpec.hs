module ReaderSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Control.Monad.Reader

import qualified Reader
import Reader (Config (Cfg))

testCompute :: (Int -> Config -> Int) -> Rubric
testCompute compute = passOrFail $ do
  it "correctly computes bogus" $ do
    compute  5 (Cfg 4   6) @?= 3
    compute 20 (Cfg 41 56) @?= 5
    compute 80 (Cfg 18  5) @?= 93
    compute 88 (Cfg 64 15) @?= 137

tests :: Rubric
tests = distribute $ do
  let unwrap rd i cfg = runReader (rd i) cfg
  distributed "compute"   $ testCompute Reader.compute
  distributed "compute'"  $ testCompute $ unwrap Reader.compute'
  distributed "compute''" $ testCompute $ unwrap Reader.compute''

