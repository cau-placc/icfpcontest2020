module Modulation where

 import Interpreter.Data
 import           Syntax

 modulate :: Integer -> [Bool]
 modulate n =
   sig
     ++ replicate len True
     ++ [False]
     ++ replicate (len * 4 - encLen) False
     ++ enc
  where
   sig | n >= 0    = [False, True]
       | otherwise = [True, False]
   enc    = encodeWithLength (abs n)
   encLen = length enc
   len    = (encLen + 3) `div` 4
 
 encodeWithLength :: Integer -> [Bool]
 encodeWithLength 0 = []
 encodeWithLength n = go n []
  where
   go 0 r  = r
   go k rs = go (div k 2) (odd k : rs)
 
 demodulateData :: [Bool] -> AlienExpr
 demodulateData dat = let (res, _) = inner dat in res
  where
   inner (False : False : rem) = (Func Nil, rem)
   inner (True : True : rem) =
     let (l, rem1) = inner rem
         (r, rem2) = inner rem1
     in  (app Cons [l,r], rem2)
   inner number =
     let i   = demodulate number
         rem = drop (length $ modulate i) number
     in  (Number i, rem)
 
 demodulate :: [Bool] -> Integer
 demodulate (s1 : s2 : bs) = sign s1 s2 * num
  where
   width = length $ takeWhile id bs
   rest  = take (4 * width) $ drop (width + 1) bs
   num   = foldl (\n b -> 2 * n + if b then 1 else 0) 0 rest
 
 sign :: Bool -> Bool -> Integer
 sign True  False = -1
 sign False True  = 1
 
 testModulation :: [Bool]
 testModulation =
   map (\n -> n == demodulate (modulate n)) (concat [ [n, -n] | n <- [0 ..] ])
 
