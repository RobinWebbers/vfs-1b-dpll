module ReaderSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Control.Monad.Reader

import qualified Reader
import Reader (Config (Cfg))

testCompute :: (Int -> Config -> Int) -> Spec
testCompute compute = 
  it "correctly computes bogus" $ do
    compute  5 (Cfg 4   6) @?= 3
    compute 20 (Cfg 41 56) @?= 5
    compute 80 (Cfg 18  5) @?= 93
    compute 88 (Cfg 64 15) @?= 137

tests :: Spec
tests = do
  let unwrap rd i cfg = runReader (rd i) cfg
  describe "compute"   $ testCompute Reader.compute
  describe "compute'"  $ testCompute $ unwrap Reader.compute'
  describe "compute''" $ testCompute $ unwrap Reader.compute''

