module ListSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import qualified List
import List (Sheep (..))

orphan, mothersInf, fathersInf, bothInf :: Sheep
orphan     = Sheep Nothing Nothing
mothersInf = Sheep (Just mothersInf) Nothing
fathersInf = Sheep Nothing (Just fathersInf)
bothInf    = Sheep (Just bothInf) (Just bothInf)

limit :: Int -> Sheep -> Sheep
limit 0 _                         = Sheep Nothing Nothing
limit n (Sheep m f) = Sheep (m >>= limit') (f >>= limit')
  where
    limit' = return . limit (n - 1)

mothersD2, fathersD2, bothD2 :: Sheep
mothersD2 = limit 2 mothersInf
fathersD2 = limit 2 fathersInf
bothD2    = limit 2 bothInf

testGgParents :: (Sheep -> [Sheep]) -> Spec
testGgParents ggParents = 
  it "lists all great grandparents" $ do
    let s0 = Sheep (Just bothD2)    (Just mothersD2)
    let s1 = Sheep (Just fathersD2) (Just mothersD2)
    let s2 = Sheep (Just fathersD2) (Just bothD2)
    ggParents orphan    @?= []
    ggParents s0        @?= replicate 5 orphan
    ggParents s1        @?= replicate 2 orphan
    ggParents s2        @?= replicate 5 orphan

tests :: Spec
tests = do
  describe "parents" $ do
    it "correct number of parents" $ do
      List.parents orphan                                @?= []
      List.parents (Sheep (Just orphan)  Nothing      )  @?= [orphan]
      List.parents (Sheep Nothing        (Just orphan))  @?= [orphan]
      List.parents (Sheep (Just orphan)  (Just orphan))  @?= [orphan, orphan]
  describe "ggParents"  $ testGgParents List.ggParents
  describe "ggParents'" $ testGgParents List.ggParents'
