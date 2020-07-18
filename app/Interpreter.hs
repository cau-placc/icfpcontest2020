{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}

module Interpreter where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Functor
import           Text.Show.Functions

import           Syntax
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )


import Modulation
import Interpreter.Data

(.:) = (.) . (.)

showData :: Data -> MIB String
showData = \case
  Int i     -> pure $ show i
  Pic l     -> pure $ "Pic(" <> show l <> ")"
  Modulated s -> pure $  "Modulated(" <> show s <> ")"
  Part f p  -> do
    res <- tryReduce f p
    case res of
      Part Nil [] -> pure "()"
      Part Cons [x,y] -> do
        l <- showData =<< runExpr x
        r <- showData =<< runExpr y
        pure $ "(" <> l <> ", " <> r <> ")"                
      Part f p    -> do
        let l = fmap show p
        pure $ "ap " <> show f <> foldl (\a b -> a ++ ' ':b ) "" l
      o       -> showData o

modulateToString :: Data -> MIB String
modulateToString dat = map (\b -> if b then '1' else '0') <$> modulateData dat

stringDemodulate :: String -> MIB Data
stringDemodulate input = if all (\c -> c == '0' || c == '1') input
  then runExpr $ demodulateData $ map (\c -> if c == '0' then False else True) input
  else throwError $ "Demodulation Error: " <> input

modulateData :: Data -> MIB [Bool]
modulateData (Int i   ) = pure $ modulate i
modulateData (Part p t)   = do
  -- repack List, to make sure we have constructors at frone
  p' <- tryReduce p (t ++ [app S [app C [Func IsNil, Func Nil], app S [app B [Func Cons, Func Car], Func Cdr]]]) -- \x -> isnil x then nil else cons (car x) (cdr x)
  case p' of
    Part Nil []     ->  pure [False,False]
    Part Cons [x,y] -> do
      h <- modulateData =<< runExpr x
      t <- modulateData =<< runExpr y
      pure $ h ++ t
    e -> do
      e' <- showData e
      throwError $ "Expeted Nil or Cons, got " <> e'


runMIB :: MIB a -> Either String a
runMIB = runIdentity . runExceptT . flip evalStateT Map.empty . unMIB -- TODO: use except

loadProg :: AlienProg -> MIB ()
loadProg (AlienProg decls) = mapM_ loadDecl decls

loadDecl :: AlienDecl -> MIB ()
loadDecl (Decl ident expr) = modify $ Map.insert ident expr

runExpr :: AlienExpr -> MIB Data
runExpr (App l r    ) = (uncurry tryReduce) =<< foldApp l [r]
runExpr (Number i   ) = pure $ Int i
runExpr (Ident  name) = runExpr =<< gets (\env -> env Map.! name)
runExpr (Func   name) = pure $ Part name []

foldApp :: AlienExpr -> [AlienExpr] -> MIB (AlienFunc, [AlienExpr])
foldApp l rs  = case l of
    App nl r -> foldApp nl $ r:rs
    Number i -> throwError $ "Unexpected Number"
    Ident  n -> do
        e <- gets (\env -> env Map.! n)
        foldApp e rs
    Func   f -> pure (f, rs)

continue :: AlienExpr -> [AlienExpr] -> MIB Data
continue x [] = runExpr x
continue x t  = (uncurry tryReduce) =<< foldApp x t

tryReduce :: AlienFunc  -> [AlienExpr] -> MIB Data
tryReduce Nil  (x:t)          = tryReduce T t
tryReduce Neg  [x]            = Int . negate <$> (runExpr >=> asInt) x
tryReduce Cons (x: y: z: t)   = continue z (x:y:t)
tryReduce K (x:y:t)           = continue x t
tryReduce S (x:y:z:t)         = continue x (y : App y x : t)
tryReduce C (x:y:z:t)         = continue x (z:y:t)
tryReduce B (x:y:z:t)         = continue x (App y z:t)
tryReduce I (x:t)             = continue x t
tryReduce T (t:_: r)          = continue t r
tryReduce F (_:f: r)          = continue f r
tryReduce Car (x:t)           = continue x (Func T: t)
tryReduce Cdr (x:t)           = continue x (Func F: t)
tryReduce Add  [x, y] = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  pure $ Int $ x' + y'
tryReduce Minus  [x, y] = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  pure $ Int $ x' - y'
tryReduce Mul  [x, y] = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  pure $ Int $ x' * y'
tryReduce Div  [x, y] = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  pure $  Int $ x' `div` y'
tryReduce Eq (x:y:t) = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  tryReduce (if x' == y' then  T else F) t
tryReduce Lt (x:y:t) = do
  x' <- (runExpr >=> asInt) x
  y' <- (runExpr >=> asInt) y
  tryReduce (if x' < y' then  T else F) t
tryReduce IF0 (x:t) = do
  x' <- (runExpr >=> asInt) x
  tryReduce (if x' == 0 then T else F) t
tryReduce Dec [x] = do
  x' <- (runExpr >=> asInt) x
  pure $ Int $ x' - 1
tryReduce Inc [x] = do
  x' <- (runExpr >=> asInt) x
  pure $ Int $ x' + 1
tryReduce Pwr2 [x] = do
  x' <- (runExpr >=> asInt) x
  pure $ Int $ 2 ^ x'
tryReduce IsNil (x:t) = do
  x' <- runExpr x
  case x' of
   Part Nil [] -> tryReduce T t
   _           -> tryReduce F t
tryReduce Interact (x2:x4:x3:t) = tryReduce F38 (x2: App (App x2 x4) x3:t)
tryReduce Modem (x0:t) = tryReduce Dem (app Mod [x0]:t)
tryReduce MultipleDraw (x0:t) = tryReduce IsNil (x0: Func Nil: (app Cons [app Draw [app Car [x0]], app MultipleDraw [app Cdr [x0]]]) : t)
tryReduce Dem [x] = do
  x' <- (runExpr >=> asModulated) x
  stringDemodulate x'
tryReduce Mod [x] = do 
  x' <- runExpr x
  Modulated <$> modulateToString x'
tryReduce Draw [v] = do
  e <- runExpr v
  l <- asList e
  lp <- mapM asPair l
  lpi <- mapM (\(f, s) -> (,) <$> asInt f <*> asInt s) lp
  pure $ Pic lpi
tryReduce Send [_] = undefined
tryReduce F38 (x2:x0:t) = tryReduce IF0
            (  app Car [x0]
            : toExprList [app Modem [app Car [app Cdr [x0]]], app MultipleDraw [app Car [app Cdr [app Cdr [x0]]]]]
            : (app Interact [x2, app Modem [app Car [app Cdr [x0]]], app Send [app Car [app Cdr [app Cdr [x0]]]]])
            : t
            )

-- can't reduce at the moment
tryReduce f p   = pure $ Part f p

toExprList :: [AlienExpr] -> AlienExpr
toExprList []      = Func Nil
toExprList (h : t) = app Cons [h, toExprList t]

asInt :: Data -> MIB Integer
asInt = \case
  Int i -> pure i
  Part f p -> do
    res <- tryReduce f p
    case res of
            Int i -> pure i
            e     -> do
              e' <- showData e
              throwError $ "expected integer, got " <> e'
  e     -> do
      e' <- showData e
      throwError $ "expected integer, got " <> e'

asPair :: Data -> MIB (Data, Data)
asPair = \case
  Part Cons [x,y] ->
    (,) <$> runExpr x <*> runExpr y
  Part f    p     -> do
    res <- tryReduce f p
    case res of
      Part Cons [x,y]   ->
          (,) <$> runExpr x <*> runExpr y
      e                 -> throwError $ "expected pair, got" <> show e
  e        -> throwError $ "expected pair, got" <> show e

asList :: Data -> MIB [Data]
asList = \case
  Part Nil [] -> pure []
  Part Cons [x,y] -> (:) <$> runExpr x <*> (asList =<< runExpr y)
  Part f p -> do
    res <- tryReduce f p
    case res of
        Part Nil [] -> pure []
        Part Cons [x,y] -> (:) <$> runExpr x <*> (asList =<< runExpr y)
        e        -> throwError $ "expected unit or pair, got" <> show e
  e        -> throwError $ "expected unit or pair, got" <> show e

asModulated :: Data -> MIB String
asModulated = \case
  Modulated m -> pure m
  _           -> throwError "Expected Modulated Data"