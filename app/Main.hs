import System.Environment
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

import Network

import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("ServerUrl: " ++ head args ++ "; PlayerKey: " ++ args !! 1)
  request (head args) "" (args !! 1) `catch` handler
  alienSend (head args) (args !! 1) `catch` handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Request failed with:\n" ++ show ex


modulate :: Integer -> [Bool]
modulate n =
  sig ++
  replicate len True ++ [False] ++
  replicate (len * 4 - encLen) False ++ enc
  where
    sig | n >= 0    = [False, True]
        | otherwise = [True, False]
    enc = encodeWithLength (abs n)
    encLen = length enc
    len = (encLen + 3) `div` 4

encodeWithLength :: Integer -> [Bool]
encodeWithLength 0 = []
encodeWithLength n = go n []
    where
      go 0 r = r
      go k rs = go (div k 2) (odd k : rs)

demodulate :: [Bool] -> Integer
demodulate (s1 : s2 : bs) = sign s1 s2 * num
  where
    width = length $ takeWhile id bs
    rest = take (4 * width) $ drop (width + 1) bs
    num = foldl (\n b -> 2 * n + if b then 1 else 0) 0 rest

sign :: Bool -> Bool -> Integer
sign True False = -1
sign False True = 1

testModulation :: [Bool]
testModulation =
  map (\n -> n == demodulate (modulate n)) (concat [ [n, -n] | n <- [0..]])

data AlienAST = Number Integer
              | NumberModulated [Bool]
              | Succ AlienAST
              | Pred AlienAST
              | Neg  AlienAST
              | Sum  AlienAST AlienAST
              | Prod AlienAST AlienAST
              | Div  AlienAST AlienAST
              | Eq   AlienAST AlienAST
              | Lq   AlienAST
              | Modulate   AlienAST
              | Demodulate AlienAST
              | Send       AlienAST
              | List       [AlienAST]
