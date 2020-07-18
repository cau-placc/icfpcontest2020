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
funcAsData Lt   = partial2 $ \l r -> do
  l' <- (runExpr >=> asInt) l
  r' <- (runExpr >=> asInt) r
  ite $ l' < r'
funcAsData func = error $ show func

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
  _         -> throwError "expected function"

asInt :: Data -> MIB Integer
asInt = \case
  Int i -> pure i
  _     -> throwError "expected integer"