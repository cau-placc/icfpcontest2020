module Interpreter.Data where

type MIBEnv = Map AlienName AlienExpr

newtype MIB a = MIB { unMIB :: StateT MIBEnv (Except String) a }
  deriving (Functor, Applicative, Monad, MonadState MIBEnv, Alternative, MonadError String)

data Data = Partial (AlienExpr -> MIB Data) | Int Integer | Pair AlienExpr AlienExpr | Unit | Pic [(Integer, Integer)] | Modulated String deriving (Show)


app :: AlienFunc -> [AlienExpr] -> AlienExpr
app f = foldl App $ Func f