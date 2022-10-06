import Test.Hspec
import Test.Hspec.Runner
import Test.HUnit
import System.Environment

-- Monad imports
import qualified MaybeSpec
import qualified ListSpec
import qualified ReaderSpec
import qualified WriterSpec
import qualified StateSpec

-- Sat imports
import qualified PropSpec
import qualified TseitinSpec
import qualified DPLLSpec

import Rubric

rubric :: Rubric
rubric = do
  criterion "Monads" 0.25 . distribute $ do
    distributed "Maybe"  MaybeSpec.tests
    distributed "List"   ListSpec.tests
    distributed "Reader" ReaderSpec.tests
    distributed "Writer" WriterSpec.tests
    distributed "State"  StateSpec.tests
  criterion "Sat" 0.75 . distribute $ do
    distributed "Prop"    PropSpec.tests
    distributed "Tseitin" TseitinSpec.tests
    distributed "DPLL"    DPLLSpec.tests

main :: IO ()
main = do
  args <- getArgs
  cfg <- readConfig defaultConfig args
  let (criteria, spec) = evalRubricM rubric
  (cfg', forest) <- evalSpec cfg spec
  result <- withArgs [] $ runSpecForest forest cfg'
  print $ grade criteria result
  evaluateResult result
