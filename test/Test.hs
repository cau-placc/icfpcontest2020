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
import AlienApi

main :: IO ()
main = do
  -- runGame
  runDocTests
  runGalaxy

runGame :: IO ()
runGame = do
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

data InteractState = InteractState {value :: Value, prog :: MIB ()}

-- runs interact from a starting state one interaction point
stepGalaxy :: InteractState -> (Integer, Integer) -> IO (InteractState, [Img])
stepGalaxy p@InteractState{value = state, prog = prog} (x,y) = do
  let point = app Cons [Number x, Number y]
  Right (newState, imgs) <- runMIB $ prog >> do
    result <- runExpr $ app Interact [Ident Galaxy, fromJust $ fromValue state, point]
    state' <- dataToValue =<< extractState result
    imgs   <- extractPics result
    pure (state', imgs)
  pure (InteractState{value = newState, prog = prog}, imgs)

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
runGalaxy' :: InteractState -> [(Integer, Integer)] -> IO (InteractState, [(InteractState, [Img])])
runGalaxy' p []    = pure (p,[])
runGalaxy' p (h:t) = do
  r@(p',d) <- stepGalaxy p h
  (fp, rs) <- runGalaxy' p' t
  pure $ (fp, r : rs)

runGalaxy :: IO ()
runGalaxy = do

  putStrLn "Testing draw (Checkerboard (7,0)) and draw (Checkerboard (13,0))"

  let Right prog2 = either (error . show) Right $ parseAlienProg "galaxy = ap ap s ap ap b s ap ap c ap ap b c ap ap b ap c ap c ap ap s ap ap b s ap ap b ap b ap ap s i i lt eq ap ap s mul i nil ap ap s ap ap b s ap ap b ap b cons ap ap s ap ap b s ap ap b ap b cons ap c div ap c ap ap s ap ap b b ap ap c ap ap b b add neg ap ap b ap s mul div ap ap c ap ap b b galaxy ap ap c add 2"
      result2 = loadProg prog2 >> runExpr (app Draw [app I [Ident Galaxy, Number  7, Number 0]])
      result3 = loadProg prog2 >> runExpr (app Draw [app I  [Ident Galaxy, Number 13, Number 0]])
  showPics =<< (pure . fromRight undefined) =<< (runMIB $ extractPics =<<result2)
  showPics =<< (pure . fromRight undefined) =<< (runMIB $ extractPics =<<result3)

  putStrLn "\nParsing Galaxy:"
  galaxy <- readFile galaxyFile
  let Right prog  = either (error . show) Right $ parseAlienProg galaxy

  if show prog == (unlines $ lines galaxy) then
    putStrLn "[Ok] Equality holds!"
  else
    putStrLn "[Warning] Expected Equality"

  putStrLn "\nRunning Galaxy:"

  if True then do -- skipping initial sequence as generating the last image takes "forever"
    let
      initState = InteractState{value = Combat.Data.Nil, prog = loadProg prog}
    result <- runGalaxy' initState  [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (8,4), (2,-8), (3,6), (0,-14), (-4,10), (9,-3), (-4,10), (0,0)]
    putStrLn "Starting from the begining, this will take some time ..."
    displayOutputs $ snd result
  else
    pure ()

  let
    -- start at [2, [1, -1], 0, nil]
    state = toValue [toValue (2::Integer), toValue [1::Integer,-1], toValue (0::Integer), Combat.Data.Nil]
    continue = [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (-18,0)]
    -- start at [5, [2, 0, nil, nil, nil, nil, nil, 0], 8, nil]
    -- state = toValue [toValue (5::Integer), toValue [toValue (2::Integer),toValue (0::Integer), Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, toValue (0::Integer)], toValue (8::Integer), Combat.Data.Nil]
    -- continue = [(-18,0)]
    initState = InteractState{value = state, prog = loadProg prog}
  result <- runGalaxy' initState continue
  displayOutputs $ snd result

  -- used for exploring next input
  if True then do
    let state = toValue [toValue (5::Integer), toValue [toValue (2::Integer),toValue (0::Integer), Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, Combat.Data.Nil, toValue (0::Integer)], toValue (8::Integer), Combat.Data.Nil]    
    putStrLn "Trying to find next Input!"
    forM_ [(-18,0), (-36,0), (-54,0), (-72,0), (-90,0), (-108,0), (72, 0)] $ \x -> do
        putStrLn $ "Trying next " <> show x
        let startState = InteractState{value = state, prog = loadProg prog}
        result <- runGalaxy' startState  [x]
        displayOutputs $ snd result
  else
    pure ()
  putStrLn "Done"

displayOutputs :: [(InteractState ,[Img])] -> IO ()
displayOutputs results = void $ mapM displayOutput results

showResult :: Data -> MIB String
-- showResult dat = (showData =<< runExpr (app Car [dataToExpr dat]))
showResult dat = show <$> (dataToValue =<< runExpr (app Car [dataToExpr dat]))

displayOutput :: (InteractState, [Img]) -> IO ()
displayOutput (InteractState{value = state}, pics) = do
  putStrLn $ "-----\nNew State: " <> (show state)
  showPics pics
  putStrLn $ "-----"

showPics :: [Img] -> IO ()
showPics pics = printDataAsDataUrlPng "output.png" pics