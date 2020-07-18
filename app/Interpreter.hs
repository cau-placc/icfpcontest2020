{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

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

(.:) = (.) . (.)

data Data = Partial (AlienExpr -> MIB Data) | Int Integer | Pair Data Data | Unit deriving (Show)

type MIBEnv = Map AlienName AlienExpr

newtype MIB a = MIB { unMIB :: StateT MIBEnv (Except String) a }
  deriving (Functor, Applicative, Monad, MonadState MIBEnv, Alternative, MonadError String)

runMIB :: MIB a -> Either String (a, MIBEnv)
runMIB = runIdentity . runExceptT . flip runStateT Map.empty . unMIB -- TODO: use except

loadProg :: AlienProg -> MIB ()
loadProg (AlienProg decls) = mapM_ loadDecl decls

loadDecl :: AlienDecl -> MIB ()
loadDecl (Decl ident expr) = modify $ Map.insert ident expr

runExpr :: AlienExpr -> MIB Data
runExpr (App l r    ) = withPartial l ($ r)
runExpr (Number i   ) = pure $ Int i
runExpr (Ident  name) = runExpr =<< gets (\env -> env Map.! name)
runExpr (Func   name) = funcAsData name
runExpr Nil           = pure Unit
runExpr Cons          = partial2 $ \x xs -> Pair <$> runExpr x <*> runExpr xs

funcAsData :: AlienFunc -> MIB Data
funcAsData Add = partial2
  $ \l r -> (Int .: (+)) <$> (runExpr >=> asInt) l <*> (runExpr >=> asInt) r
funcAsData Neg = partial1 $ \e -> Int . negate <$> (runExpr >=> asInt) e
funcAsData Eq  = partial2 $ \l r -> do
  l' <- (runExpr >=> asInt) l
  r' <- (runExpr >=> asInt) r
  ite $ l' == r'
funcAsData K    = partial2 $ \x y -> runExpr x
funcAsData S    = partial3 $ \x y z -> runExpr $ App (App x z) (App y z)
funcAsData C    = partial3 $ \x y z -> runExpr $ App (App x z) y
funcAsData B    = partial3 $ \x y z -> runExpr $ App x $ App y z
funcAsData I    = partial1 $ \x -> runExpr x
funcAsData T    = ite True
funcAsData F    = ite False
funcAsData Dec  = partial1 $ \e -> Int . (\x -> x - 1) <$> (runExpr >=> asInt) e
funcAsData Inc  = partial1 $ \e -> Int . (+ 1) <$> (runExpr >=> asInt) e
funcAsData Pwr2 = partial1 $ \e -> Int . (\x -> 2 ^ x) <$> (runExpr >=> asInt) e
funcAsData IsNil = partial1 $ \p -> do
  d <- runExpr p
  case d of
    Unit      -> funcAsData T
    Pair _ _  -> funcAsData F
    Int _     -> funcAsData F
    -- Partial f -> f $ partial2 $ \_ _ -> funcAsData F -- TODO

funcAsData Lt   = partial2 $ \l r -> do
  l' <- (runExpr >=> asInt) l
  r' <- (runExpr >=> asInt) r
  ite $ l' < r'
-- TODO ap car x2 = ap x2 t
funcAsData Car  = partial1 $ \x -> do
  (h,_) <- (runExpr >=> asPair) x
  pure $ h
-- TODO ap cdr x2 = ap x2 f
funcAsData Cdr  = partial1 $ \x -> do
  (_,t) <- (runExpr >=> asPair) x
  pure $ t
funcAsData Mul = partial2 $ \l r -> do
  l' <- (runExpr >=> asInt) l
  r' <- (runExpr >=> asInt) r
  pure $ Int $ l' + r'
funcAsData Div = partial2 $ \l r -> do
   l' <- (runExpr >=> asInt) l
   r' <- (runExpr >=> asInt) r
   pure $ Int $ l' `div` r'
funcAsData Interact = alienInteract
funcAsData Modem = modem
funcAsData F38 = f38
funcAsData func = error $ show func

alienInteract = partial3 $ \prot state vec -> runExpr $ App (App (Func F38) prot) (App (App prot state) vec)
f38 = partial2 $ \prot state -> runExpr $ App
  (App (App (Func IF0) (App (Func Car) state)) $ toExprList [App (Func Modem) (App (Func Car) (App (Func Cdr) state)), App (Func Multidraw) (App (Func Car) (App (Func Cdr) (App (Func Cdr) state)))])
  $ App (App (App (Func Interact) prot) (App (Func Modem) (App (Func Car) (App (Func Cdr) state)))) (App (Func Send) (App (Func Car) (App (Func Cdr) (App (Func Cdr) state))))
modem = partial1 $ \x -> runExpr $ App (Func Dem) (App (Func Mod) x) 

toExprList :: [AlienExpr] -> AlienExpr
toExprList [] = Nil
toExprList (h:t) = App (App Cons h) $ toExprList t


ite :: Bool -> MIB Data
ite cond = partial2 $ \t f -> runExpr $ if cond then t else f

partial1 :: (AlienExpr -> MIB Data) -> MIB Data
partial1 = pure . Partial

partial2 :: (AlienExpr -> AlienExpr -> MIB Data) -> MIB Data
partial2 k = pure $ Partial $ \x -> pure $ Partial $ \y -> k x y

partial3 :: (AlienExpr -> AlienExpr -> AlienExpr -> MIB Data) -> MIB Data
partial3 k =
  pure $ Partial $ \x -> pure $ Partial $ \y -> pure $ Partial $ \z -> k x y z

withPartial :: AlienExpr -> ((AlienExpr -> MIB Data) -> MIB Data) -> MIB Data
withPartial expr k = runExpr expr >>= \case
  Partial f -> k f
  Unit      -> funcAsData T
  _         -> throwError "expected function"

asInt :: Data -> MIB Integer
asInt = \case
  Int i -> pure i
  _     -> throwError "expected integer"

asPair :: Data -> MIB (Data, Data)
asPair = \case
  Pair x y -> pure (x, y)
  _        -> throwError "expected pair"