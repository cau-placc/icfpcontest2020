import           Control.Monad (void)
import           Data.Either (fromRight)

import           Parser hiding (app)
import           Renderer
import           Syntax
import           Interpreter
import           Interpreter.Data

galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"
statefulFile = "stateful.txt"


emptyList = (Func Nil)
list0 = app Cons [Number 0, emptyList]
list1 = app Cons [Number 0, app Cons [Number 0]]
tuple0 = app Cons [Number 0, Number 0]

main :: IO ()
main = do
  galaxy <- readFile galaxyFile
  let code        = unlines $ lines galaxy
      Right prog  = either (error . show) Right $ parseAlienProg code
      result      = loadProg prog >> runExpr
        (app Interact [Ident Galaxy, emptyList, tuple0])
      shownResult = runMIB $ showData =<< result
      pics        = runMIB $ extractPics =<< result
  
  putStrLn $ "-----\nResult: " <> show shownResult
  void
    $ mapM (\(i, d) -> renderDataAsPngTo ("output" <> show i <> ".png") d)
    $ zip [0..]
    $ filter (\(Pic px) -> not $ null px)
    $ fromRight undefined pics
