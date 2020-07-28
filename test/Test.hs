import           Control.Concurrent
import           Control.Monad (void,forM_)
import           Data.Either (fromRight)
import           Data.Maybe (fromJust)

import           Parser (parseAlienProg)
import           Renderer
import           Syntax
import           Interpreter
import           Interpreter.Data
import           Combat
import           Combat.Data
import           AlienNetwork
import           Modulation
import           Executor

import           DocTest
import qualified AlienApi

main :: IO ()
main = do
  -- runGame
  runDocTests
  runGalaxy

runGame :: IO ()
runGame = do
    (attack, defence) <- create AlienApi.server AlienApi.apiKey
    void $ forkIO $ do
        Combat.init attack
    Combat.init defence

create :: String -> String -> IO (Connection, Connection)
create server apiKey = do
    let createRequest = bitsToString $ modulateValue $ toValue [1::Integer,0]
    putStrLn $"Create Request: " <> createRequest
    Right response <- postAlien (Connection server 0 $ Just apiKey) createRequest
    let value = demodulateValue $ fromJust $ stringToBits response
    putStrLn $ show $ value
    let Just (CreateResponse attack defence) = tryFromValue value
    pure $ (Connection server attack $ Just apiKey, Connection server defence $ Just apiKey)

data CreateResponse = CreateResponse Integer Integer

instance TryFromValue CreateResponse where
  tryFromValue v = do
      (Num 1, [(Num 0, attackKey), (Num 1, defenceKey)]) <- tryFromValue v
      pure $ CreateResponse attackKey defenceKey



-- runs interact from a starting state for the sequence of interaction points
-- returning the final state and all results
runGalaxy' :: InteractState -> [(Integer, Integer)] -> IO (InteractState, [(InteractState, [Img])])
runGalaxy' p []    = pure (p,[])
runGalaxy' p (h:t) = do
  r@(p', _) <- stepGalaxy p h
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
  galaxy <- readFile AlienApi.galaxyFile
  let Right prog  = either (error . show) Right $ parseAlienProg galaxy

  if show prog == (unlines $ lines galaxy) then
    putStrLn "[Ok] Equality holds!"
  else
    putStrLn "[Warning] Expected Equality"

  putStrLn "\nRunning Galaxy:"

  if True then do -- skipping initial sequence as generating the last image takes "forever"
    runInitializationSequence prog
  else
    pure ()

  runStartSequence prog

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

-- expects prog to be parsed galaxy.txt
runInitializationSequence :: AlienProg -> IO ()
runInitializationSequence prog = do
  let
    initState = InteractState{value = Combat.Data.Nil, prog = loadProg prog}
    inputs = [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (8,4), (2,-8), (3,6), (0,-14), (-4,10), (9,-3), (-4,10), (0,0)] :: [(Integer,Integer)]
  -- ends at [2, [1, -1], 0, nil]
  result <- runGalaxy' initState inputs
  putStrLn "Starting from the begining, this will take some time ..."
  displayOutputs $ snd result

-- expects prog to be parsed galaxy.txt
runStartSequence :: AlienProg -> IO ()
runStartSequence prog = do
  let
    -- start at [2, [1, -1], 0, nil]
    state = toValue [toValue (2::Integer), toValue [1::Integer,-1], toValue (0::Integer), Combat.Data.Nil]
    continue = [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0)] :: [(Integer,Integer)]
    -- ends at [5, [2, 0, nil, nil, nil, nil, nil, 0], 8, nil]
    initState = InteractState{value = state, prog = loadProg prog}
  result <- runGalaxy' initState continue
  displayOutputs $ snd result


displayOutputs :: [(InteractState ,[Img])] -> IO ()
displayOutputs results = void $ mapM displayOutput results

displayOutput :: (InteractState, [Img]) -> IO ()
displayOutput (InteractState{value = state}, pics) = do
  putStrLn $ "-----\nNew State: " <> (show state)
  showPics pics
  putStrLn $ "-----"

showPics :: [Img] -> IO ()
showPics pics = printDataAsDataUrlPng "output.png" pics