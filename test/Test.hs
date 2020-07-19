import           Control.Concurrent

import           Parser hiding (app)
import           Syntax
import           Interpreter
import           Interpreter.Data
import           Combat
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
    Right response <- post server ("/aliens/send?apiKey=" <> apiKey) $ modulateValue $ toValue [0::Integer,1]
    let Just (CreateResponse attack defence) = fromValue $ demodulateValue response
    pure $ (Connection server attack $ Just apiKey, Connection server attack $ Just apiKey)

data CreateResponse = CreateResponse Integer Integer

instance FromValue CreateResponse where
  fromValue v = do
      (Num 1, [((Num 0, attackKey), (Num 1, defenceKey))]) <- fromValue v
      pure $ CreateResponse attackKey defenceKey
{-
galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"
statefullFile = "statefull.txt"


emptyList = (Func Nil)
list0 = app Cons [Number 0, emptyList]
list1 = app Cons [Number 0, app Cons [Number 0]]
tuple0 = app Cons [Number 0, Number 0]

main :: IO ()
main = do
  galaxy <- readFile galaxyFile
  let code       = unlines $ lines galaxy
      Right prog = either (error . show) Right $ parseAlienProg code
      result     = runMIB $ loadProg prog >> runExpr
        (app Interact [Ident Galaxy, emptyList, tuple0])
        >>= showData
  putStrLn $ "-----\nResult: " <> show result
-}

