{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}

module Interpreter (runMIB, runExpr, tryReduce, loadProg, showData, dataToValue) where

import           Control.Monad.State
import           Control.Monad.Except
import           Control.Concurrent.MVar
import qualified Data.Map                      as Map
import           Data.Maybe (fromJust)

import           Syntax
import           Combat
import           AlienApi
import           AlienNetwork (postAlien, Connection(..))

import Modulation
import Interpreter.Data
import qualified Combat.Data

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
      Part f' p'    -> do
        let l = fmap show p'
        pure $ "ap " <> show f' <> foldl (\a b -> a ++ ' ':b ) "" l
      o       -> showData o

modulateToString :: Data -> MIB String
modulateToString dat = bitsToString <$> modulateData dat

stringDemodulate :: String -> MIB Data
stringDemodulate input = case stringToBits input of
  Nothing -> throwError $ "Demodulation Error: " <> input
  Just bits ->  runExpr $ Combat.Data.fromValue $ demodulateValue bits

dataToExpr :: Data -> AlienExpr
dataToExpr (Int       i) = Number i
dataToExpr (Part f    p) = app f p
dataToExpr (Modulated s) = let
    Just bits = stringToBits s
  in
    app Mod [Combat.Data.fromValue $ demodulateValue bits]
dataToExpr (Pic       pixel) = app Draw [Combat.Data.fromValue $ Combat.Data.toValue pixel]

dataToValue :: Data -> MIB Combat.Data.Value
dataToValue (Pic        _) = throwError "Can't convert Data.Pic to Value"
dataToValue (Modulated  _) = throwError "Can't convert Data.Modulated to Value"
dataToValue (Int           i) = pure $ Combat.Data.Num i
dataToValue (Part    f     p) = do
  result <- tryReduce f p
  case result of
    (Part Nil     []) -> pure Combat.Data.Nil
    (Part Cons [h,t]) -> Combat.Data.Pair <$> (dataToValue =<< runExpr h) <*> (dataToValue =<< runExpr t)
    e -> do
      e' <- showData e
      throwError $ "Expected Nil or Cons, got " <> e'

modulateData :: Data -> MIB [Bool]
modulateData (Modulated _  ) = throwError "Can't modulate Data.Modulated"
modulateData (Pic       _  ) = throwError "Can't modulate Data.Pic"
modulateData (Int       i  ) = pure $modulateValue $ Combat.Data.Num i
modulateData (Part      f p) = do
  f' <- tryReduce f p
  case f' of
    Part Nil []     ->  pure [False,False]
    Part Cons [x,y] -> do
      h <- modulateData =<< runExpr x
      t <- modulateData =<< runExpr y
      pure $ [True, True] ++ h ++ t
    e -> do
      e' <- showData e
      throwError $ "Expected Nil or Cons, got " <> e'

runMIB :: MIB a -> IO (Either String a)
runMIB = runExceptT . flip evalStateT Map.empty . unMIB

loadProg :: AlienProg -> MIB ()
loadProg (AlienProg decls) = mapM_ loadDecl decls

loadDecl :: AlienDecl -> MIB ()
loadDecl (Decl ident expr) = modify $  Map.insert ident expr

addShared :: AlienExpr -> MIB AlienExpr
addShared expr = do
  var <- liftIO $ newMVar expr
  pure $ Shared var

runIdent :: AlienName -> MIB Data
runIdent name =  do
  expr <- runExpr =<< getIdent name
  loadDecl (Decl name $ dataToExpr expr)
  pure $ expr

runShared :: MVar AlienExpr -> MIB Data
runShared var = do
  expr <- runExpr =<< (liftIO $ takeMVar var)
  liftIO $ putMVar var (dataToExpr expr)
  pure expr

runExpr ::  AlienExpr -> MIB Data
runExpr (App l r    ) = (uncurry tryReduce) =<< foldApp l [r]
runExpr (Number i   ) = pure $ Int i
runExpr (Shared var ) = runShared var
runExpr (Ident  name) = runIdent name
runExpr (Func   name) = pure $ Part name []

getIdent :: AlienName -> MIB AlienExpr
getIdent name = gets (\env -> Map.findWithDefault (error $ "Unknown element " <> show name <> " in " <> show env) name env) -- TODO use throwError instead of error

foldApp ::  AlienExpr -> [AlienExpr] -> MIB (AlienFunc, [AlienExpr])
foldApp l rs  = case l of
    App nl r -> foldApp nl $ r:rs
    Number _ -> throwError $ "Unexpected Number, tries to apply parameters: " <> show rs
    Shared v -> do 
        e <- runShared v
        foldApp (dataToExpr e) rs
    Ident  n -> do
        e <- runIdent n
        foldApp (dataToExpr e) rs
    Func   f -> pure (f, rs)

continue ::  AlienExpr -> [AlienExpr] -> MIB Data
continue x          []    = runExpr x
continue (Number _) (_:_) = throwError "Continue on number with non empty parameter list!"
continue x          t     = (uncurry tryReduce) =<< foldApp x t

tryReduce ::  AlienFunc  -> [AlienExpr] -> MIB Data
tryReduce Nil  (_:t)          = tryReduce T t
tryReduce Neg  [x]            = Int . negate <$> (runExpr >=> asInt) x
tryReduce Cons (x: y: z: t)   = continue z (x:y:t)
--tryReduce K (x:y:t)           = continue x t -- See T
tryReduce S (x:y:z:t)         = do
  z' <- addShared z
  continue x (z' : App y z' : t)
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
  pure $  Int $ x' `quot` y'
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
tryReduce Send (x:t) = do
  dat <- modulateToString =<< runExpr x
  response <- liftIO $ postAlien (Connection server 0 (Just apiKey)) dat
  let
    Right res = response
    res' = Combat.Data.fromValue $ demodulateValue $ fromJust $ stringToBits res
  continue res' t
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
