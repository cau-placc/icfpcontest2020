import           Parser
import           Syntax
import           Interpreter

main :: IO ()
main = do
  galaxy <- readFile "galaxy.txt"
  let code       = unlines $ lines galaxy
      Right prog = parseAlienProg code
      result     = runMIB $ loadProg prog >> runExpr
        (App (App (App (Func Interact) (Ident Galaxy)) (Func Nil))
             (App (App (Func Cons) (Number 1)) (Number 2))
        )
  putStrLn $ "-----\nResult: " <> show result

