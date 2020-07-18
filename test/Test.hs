import           Parser hiding (app)
import           Syntax
import           Interpreter
import           Interpreter.Data

galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"
statefullFile = "statefull.txt"


emptyList = (Func Nil)
list0 = app Cons [Number 0, emptyList]
tuple0 = app Cons [Number 0, Number 0]

main :: IO ()
main = do
  galaxy <- readFile galaxyFile
  let code       = unlines $ lines statefullFile
      Right prog = parseAlienProg code
      result     = runMIB $ loadProg prog >> runExpr
        (app Interact [Ident Galaxy, list0, tuple0])
        >>= showData
  putStrLn $ "-----\nResult: " <> show result

