import           Parser
import           Syntax
import           Interpreter

galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"

main :: IO ()
main = do
  galaxy <- readFile statelessFile
  let code       = unlines $ lines galaxy
      Right prog = parseAlienProg code
      result     = runMIB $ loadProg prog >> runExpr
        (App (App (App (Func Interact) (Ident Galaxy)) (Func Nil))
             (App (App (Func Cons) (Number 0)) (Number 0))
        ) >> showData
  putStrLn $ "-----\nResult: " <> result

