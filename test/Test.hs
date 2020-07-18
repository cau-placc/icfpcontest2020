import           Parser
import           Syntax
import           Interpreter

main :: IO ()
main = do
  galaxy <- readFile "galaxy.txt"
  let
    code       = unlines $ lines galaxy
    Right prog = parseAlienProg code
    Right (result, _) =
      runMIB $ loadProg prog >> runExpr (Ident Galaxy)
  putStrLn $ "\nResult: " <> show result

