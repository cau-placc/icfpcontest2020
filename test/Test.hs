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
      runMIB $ loadProg prog >> runExpr (App (App (App (Func Interact) (Ident Galaxy)) Nil) (App (App Cons (Number 0)) (Number 0)))
  putStrLn $ "-----\nResult: " <> show result

