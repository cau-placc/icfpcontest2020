module Syntax where

type AlienName = Integer

data AlienDecl = Decl { declIdent :: Integer, declExpr :: AlienExpr } | Galaxy { declExpr :: AlienExpr }  deriving (Show)

newtype AlienProg = AlienProg { unAlienProg :: [AlienDecl] } deriving (Show)

data AlienExpr = App AlienExpr AlienExpr
               | Number Integer
               | Ident AlienName
               | Nil
               | Cons
               | Func AlienFunc deriving (Show, Eq)

data AlienFunc = Lt | Eq | S | T | K | I | B | C | Neg | Add | Mul | Div | Minus | IsNil | Car | Cdr deriving (Show, Eq)
