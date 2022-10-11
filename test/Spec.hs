import Test.Hspec
import Test.Hspec.Runner
import Test.Hrubric
import Test.HUnit

import System.Environment
import System.Console.ANSI
import Text.Printf

import Control.Monad
import Data.Maybe

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

-- Output the weight as grade
output :: Float -> IO ()
output g = do
  let adj = min 1 (0.1 + g * 0.9)
  let color = if adj > 0.55 then Green else Red
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR [SetColor Foreground Vivid color]
  putStr $ printf "%.1f" (adj * 10)
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStr $ "/10.0]\n"
  setSGR [Reset]

  -- Output the weight as a grade between 0 and 1 for codegrade
  codegrade <- lookupEnv "CG_INFO"
  when (isJust codegrade) $ putStrLn (printf "%.2f" adj)

main :: IO ()
main = do
  result <- hrubric rubric
  case result of
    Left p -> putStrLn $ "Error in rubric nesting: '" ++ p ++ "'"
    Right g -> output g
