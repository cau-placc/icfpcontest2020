module Executor (InteractState(..), Img(..), stepGalaxy, extractPics, extractState) where

import Control.Monad (join)

import Combat.Data
import Interpreter
import Interpreter.Data
import Syntax


data InteractState = InteractState {value :: Value, prog :: MIB ()}

data Img = Img [(Integer, Integer)]

-- runs interact from a starting state one interaction point
stepGalaxy :: InteractState -> (Integer, Integer) -> IO (InteractState, [Img])
stepGalaxy InteractState{value = state, prog = prog} (x,y) = do
  let point = app Cons [Number x, Number y]
  Right (newState, imgs) <- runMIB $ prog >> do
    result <- runExpr $ app Interact [Ident Galaxy, fromValue state, point]
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
    Part Cons [f',_] -> runExpr f'
    d               -> do
      r <- showData d
      return $ error $ "Expected a Part Cons [state,_], got " <> r
extractState d = do
      r <- showData d
      return $ error $ "Expected a Part Cons [state,_], got " <> r

-- Extracts the pictures from the given data
extractPics :: Data -> MIB [Img]
extractPics (Pic pxs)  = pure $ [Img pxs]
extractPics (Part f p) = do
  res <- tryReduce f p
  case res of
    Part _ es -> join <$> (mapM extractPics =<< mapM runExpr es)
    _         -> pure []
extractPics _          = pure $ []