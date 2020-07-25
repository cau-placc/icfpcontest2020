module Syntax where

import           Control.Concurrent.MVar
import           Data.Char                      ( toLower )

data AlienName = FuncName Integer | Galaxy deriving (Eq, Ord)

data AlienDecl = Decl { declIdent :: AlienName, declExpr :: AlienExpr }

newtype AlienProg = AlienProg { unAlienProg :: [AlienDecl] }

data AlienExpr = App AlienExpr AlienExpr
               | Number Integer
               | Ident AlienName
               | Func AlienFunc
               | Shared (MVar AlienExpr)
               deriving (Eq)

data AlienFunc = Lt | Eq | S | T | F | K | I | B | C | IF0 | Dem | Mod | MultipleDraw | Draw | Interact | Send | Modem | F38 | Neg | Add | Mul | Div | Dec | Pwr2 | Inc | Minus | IsNil | Car | Cdr | Nil | Cons deriving (Show, Eq)


instance Show AlienProg where
  show AlienProg{unAlienProg = unAlienProg} = unlines $ map show unAlienProg

instance Show AlienDecl where
  show Decl{declIdent = declIdent, declExpr = declExpr } = show declIdent <> " = " <> show declExpr

instance Show AlienExpr where
  show (App    f s ) = "ap " <> show f <> " " <> show s
  show (Number i   ) = show i
  show (Ident ident) = show ident
  show (Func  func ) = map toLower $ show func
  show (Shared _   ) = "(Shared)"

instance Show AlienName where
  show Galaxy       = "galaxy"
  show (FuncName i) = ':' : show i
