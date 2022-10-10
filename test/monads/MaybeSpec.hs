module MaybeSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import qualified Maybe

testOneEighth :: (Int -> Maybe Int) -> Rubric
testOneEighth oneEighth = passOrFail $ do
  it "correctly computes" $ do
    oneEighth   8 @?= Just 1
    oneEighth  64 @?= Just 8
    oneEighth 328 @?= Just 41
    oneEighth   9 @?= Nothing
    oneEighth  36 @?= Nothing
    oneEighth 353 @?= Nothing

tests :: Rubric
tests = distribute $ do
  distributed "half" . passOrFail $ do
    it "takes half if even" $ do
      Maybe.half  8 @?= Just  4
      Maybe.half 32 @?= Just 16
      Maybe.half 64 @?= Just 32
    it "returns nothing if odd" $ do
      Maybe.half  7 @?= Nothing
      Maybe.half 51 @?= Nothing
      Maybe.half 23 @?= Nothing
  distributed "oneEighth"   $ testOneEighth Maybe.oneEighth
  distributed "oneEighth'"  $ testOneEighth Maybe.oneEighth'
  distributed "oneEighth''" $ testOneEighth Maybe.oneEighth''
