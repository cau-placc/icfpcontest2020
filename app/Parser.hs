module Parser where

import           Control.Applicative            ( Alternative(some) )

import           Data.Functor                   ( ($>) )
import           Data.Foldable                  ( asum )
import           Data.Char                      ( toLower )

import           Text.Parsec

import           Syntax

type AlienParser = Parsec String ()

parseAlienProg :: String -> Either ParseError AlienProg
parseAlienProg = runParser alienProg () "Aliens"

alienProg :: AlienParser AlienProg
alienProg = AlienProg <$> some (decl <* many space) <* eof

decl :: AlienParser AlienDecl
decl = Decl <$> name  <*> (string " =" *> expr)

expr :: AlienParser AlienExpr
expr = char ' ' >> (number <|> app <|> nil <|> cons <|> ident <|> func)

number :: AlienParser AlienExpr
number = Number <$> integer

integer :: AlienParser Integer
integer = read <$> (many1 digit <|> (:) <$> char '-' <*> many digit)

app :: AlienParser AlienExpr
app = try $ string "ap" >> App <$> expr <*> expr

name :: AlienParser AlienName
name = string "galaxy" $> Galaxy <|> (char ':' >> FuncName <$> integer)

ident :: AlienParser AlienExpr
ident = Ident <$> name

nil :: AlienParser AlienExpr
nil = try $ string "nil" $> Nil

cons :: AlienParser AlienExpr
cons = try $ string "cons" $> Cons

func :: AlienParser AlienExpr
func =
  Func <$> (asum $ map (\c -> try $ string (lowercase $ show c) $> c) funcs)
 where
  funcs =
    [IsNil, Neg, Add, Div, Mul, Minus, Car, Cdr, Eq, Lt, K, I, S, T, B, C]
  lowercase = map toLower

