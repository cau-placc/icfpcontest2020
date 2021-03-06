module Syntax where

data AlienName = FuncName Integer | Galaxy deriving (Show, Eq, Ord)

data AlienDecl = Decl { declIdent :: AlienName, declExpr :: AlienExpr } deriving (Show)

newtype AlienProg = AlienProg { unAlienProg :: [AlienDecl] } deriving (Show)

data AlienExpr = App AlienExpr AlienExpr
               | Number Integer
               | Ident AlienName
               | Func AlienFunc deriving (Show, Eq)

data AlienFunc = Lt | Eq | S | T | F | K | I | B | C | IF0 | Dem | Mod | MultipleDraw | Draw | Interact | Send | Modem | F38 | Neg | Add | Mul | Div | Dec | Pwr2 | Inc | Minus | IsNil | Car | Cdr | Nil | Cons deriving (Show, Eq)
