{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}
module Interpreter.Data (module Interpreter.Data) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Map                       ( Map )

import           Syntax

type MIBEnv = Map AlienName AlienExpr

newtype MIB a = MIB { unMIB :: StateT MIBEnv (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadState MIBEnv, MonadIO,  Alternative, MonadError String)

data Data = Part AlienFunc [AlienExpr] | Int Integer | Pic [(Integer, Integer)] | Modulated [Bool] deriving (Show)


app :: AlienFunc -> [AlienExpr] -> AlienExpr
app f = foldl App $ Func f