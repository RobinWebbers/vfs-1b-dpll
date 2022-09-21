import Parser
import Tseitin
import DPLL

main :: IO ()
main = do
  raw <- getLine
  let p = parse raw
  putStrLn $ if satisfiable $ equisat p
    then "SAT"
    else "UNSAT"
