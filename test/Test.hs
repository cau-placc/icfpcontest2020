import           Control.Concurrent
import           Control.Monad (void,forM,forM_)
import           Data.Either (fromRight)
import           Data.Maybe (fromJust)

import           Parser hiding (app)
import           Renderer
import           Syntax
import           Interpreter
import           Interpreter.Data
import           Combat
import           Combat.Data
import           AlienNetwork

import DocTest

main :: IO ()
main = do
  -- runGame
  runDocTests
  runGalaxy

runGame :: IO ()
runGame = do
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

galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"
statefulFile = "stateful.txt"

alienList [] = Part Syntax.Nil []
alienList (h:t) = Part Cons [Number h, dataToExpr $ alienList t]

data InteractState = InteractState {value :: Data}

-- runs interact from a starting state one interaction point
stepGalaxy :: InteractState -> (Integer, Integer) -> MIB (InteractState, Data)
stepGalaxy p@InteractState{value = state} (x,y) = do
  let point = app Cons [Number x, Number y]
  result <- runExpr $ app Interact [Ident Galaxy, dataToExpr state, point]
  state' <- extractState result
  pure (InteractState{value = state'}, result)

-- Takes the result of evaluating interact and extracts the state part
extractState :: Data -> MIB Data
extractState (Part Cons [f,_]) = runExpr f
extractState (Part f p) = do
  res <- tryReduce f p
  case res of
    Part Cons [f,_] -> runExpr f
    d               -> do
      r <- showData d
      return $ error $ "Expected a Part Cons [state,_], got " <> r
extractState d = do
      r <- showData d
      return $ error $ "Expected a Part Cons [state,_], got " <> r

-- runs interact from a starting state for the sequence of interaction points
-- returning the final state and all results
runGalaxy' :: InteractState -> [(Integer, Integer)] -> MIB (InteractState, [Data])
runGalaxy' p []    = pure (p,[])
runGalaxy' p (h:t) = do
  (p',d) <- stepGalaxy p h
  (fp, ds) <- runGalaxy' p' t
  pure $ (fp, d : ds)

runGalaxy :: IO ()
runGalaxy = do

  putStrLn "Testing draw (Checkerboard (7,0)) and draw (Checkerboard (13,0))"

  let Right prog2 = either (error . show) Right $ parseAlienProg "galaxy = ap ap s ap ap b s ap ap c ap ap b c ap ap b ap c ap c ap ap s ap ap b s ap ap b ap b ap ap s i i lt eq ap ap s mul i nil ap ap s ap ap b s ap ap b ap b cons ap ap s ap ap b s ap ap b ap b cons ap c div ap c ap ap s ap ap b b ap ap c ap ap b b add neg ap ap b ap s mul div ap ap c ap ap b b galaxy ap ap c add 2"
      result2 = loadProg prog2 >> runExpr (app Draw [app I [Ident Galaxy, Number  7, Number 0]])
      result3 = loadProg prog2 >> runExpr (app Draw [app I  [Ident Galaxy, Number 13, Number 0]])
  showPics $ fromRight undefined $ runMIB $ extractPics =<<result2
  showPics $ fromRight undefined $ runMIB $ extractPics =<<result3

  putStrLn "\nParsing Galaxy:"
  galaxy <- readFile galaxyFile
  let Right prog  = either (error . show) Right $ parseAlienProg galaxy

  if show prog == (unlines $ lines galaxy) then
    putStrLn "[Ok] Equality holds!"
  else
    putStrLn "[Warning] Expected Equality"

  putStrLn "\nRunning Galaxy:"
  let
    initState = InteractState{value = alienList []}
    --
    result = loadProg prog >> runGalaxy' initState  [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (8,4), (2,-8), (3,6), (0,-14), (-4,10), (9,-3), (-4,10)]
  displayOutputs $ (pure . snd) =<<result

  forM_ [(i,j)| i <- [-16..16] , j <- [-16..16]] $ \x -> do
      putStrLn $ "Trying next " <> show x
      let startState = InteractState{value = fromJust $ fromValue $ toValue [toValue (10::Integer),toValue [(7::Integer)], toValue (0::Integer), Combat.Data.Nil]}
          result = loadProg prog >> runGalaxy' startState  [x]
      displayOutputs $ (pure . snd) =<< result


  putStrLn "Done"

displayOutputs :: MIB [Data] -> IO ()
displayOutputs results = do
  void $ mapM displayOutput' $ fromRight undefined $ runMIB $ do
      results' <- results
      forM results' $ \result -> do
        shown <- showResult result
        pics  <- extractPics result
        pure (shown, pics)

displayOutput :: MIB Data -> IO ()
displayOutput result = do
  displayOutput' $ fromRight undefined $ runMIB $ do
    shown <- showResult =<< result
    pics  <- extractPics =<< result
    pure (shown, pics)

showResult :: Data -> MIB String
showResult dat = (showData =<< runExpr (app Car [dataToExpr dat]))
--showResult dat = show <$> (dataToValue =<< runExpr (app Car [dataToExpr dat]))

displayOutput' :: (String, [Data]) -> IO ()
displayOutput' (shownResult, pics) = do
  putStrLn $ "-----\nNew State: " <> show shownResult
  showPics pics

showPics :: [Data] -> IO ()
showPics pics = void
                    $ mapM (\(i, d) -> printDataAsDataUrlPng ("output" <> show i <> ".png") d)
                    $ zip [0..]
                    $ filter (\(Pic px) -> not $ null px) pics