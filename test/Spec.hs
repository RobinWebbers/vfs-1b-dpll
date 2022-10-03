import Test.Hspec
import Test.Hspec.Runner
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

monads :: Spec
monads = do
  describe "Monads" $ do
    describe "Maybe"  MaybeSpec.tests
    describe "List"   ListSpec.tests
    describe "Reader" ReaderSpec.tests
    describe "Writer" WriterSpec.tests
    describe "State"  StateSpec.tests

sat :: Spec
sat = do
  describe "Sat" $ do
    describe "Prop"    PropSpec.tests
    describe "Tseitin" TseitinSpec.tests
    describe "DPLL"    DPLLSpec.tests

tests :: Spec
tests = monads >> sat

main :: IO ()
main = do
  args <- getArgs
  cfg <- readConfig defaultConfig args
  (cfg', spec) <- evalSpec cfg tests
  result <- withArgs [] $ runSpecForest spec cfg'
  print result
  evaluateResult result
