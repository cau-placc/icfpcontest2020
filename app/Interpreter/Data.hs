{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}
module Interpreter.Data where

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

type MIBEnv = (Map AlienName AlienExpr, Integer)

newtype MIB a = MIB { unMIB :: StateT MIBEnv (Except String) a }
  deriving (Functor, Applicative, Monad, MonadState MIBEnv, Alternative, MonadError String)

data Data = Part AlienFunc [AlienExpr] | Int Integer | Pic [(Integer, Integer)] | Modulated String deriving (Show)


app :: AlienFunc -> [AlienExpr] -> AlienExpr
app f = foldl App $ Func f