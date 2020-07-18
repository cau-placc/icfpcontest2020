import           Control.Concurrent
import           Control.Monad (void)
import           Data.Either (fromRight)

import           Parser hiding (app)
import           Renderer
import           Syntax
import           Interpreter
import           Interpreter.Data
import           Combat
import           Combat.Data
import           AlienNetwork

main :: IO ()
main = do
  let apiKey = "e5a6755f75374b6fbb621ae3d46e6f36"
      server = "https://icfpc2020-api.testkontur.ru"
  (attack, defence) <- create server apiKey
  forkIO $ do
      Combat.init attack
  Combat.init defence

create :: String -> String -> IO (Connection, Connection)
create server apiKey = do
    let createRequest = modulateValue $ toValue [1::Integer,0]
    putStrLn $"Create Request: " <> createRequest
    Right response <- post server ("/aliens/send?apiKey=" <> apiKey) createRequest
    putStrLn $ show $ demodulateValue response
    let Just (CreateResponse attack defence) = fromValue $ demodulateValue response
    pure $ (Connection server attack $ Just apiKey, Connection server defence $ Just apiKey)

data CreateResponse = CreateResponse Integer Integer

instance FromValue CreateResponse where
  fromValue v = do
      (Num 1, [(Num 0, attackKey), (Num 1, defenceKey)]) <- fromValue v
      pure $ CreateResponse attackKey defenceKey
{-
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
-}

