module Modulation (bitsToString, stringToBits, modulateValue, demodulateValue) where

 import           Prelude hiding (rem)
 import           Debug.Trace
 import           Combat.Data

 bitsToString :: [Bool] -> String
 bitsToString []        = []
 bitsToString (False:t) = '0' : bitsToString t
 bitsToString (True :t) = '1' : bitsToString t

 stringToBits :: String -> Maybe [Bool]
 stringToBits []        = pure $ []
 stringToBits ('0': t)  = do
    t' <- stringToBits t
    pure $ False : t'
 stringToBits ('1': t)  = do
    t' <- stringToBits t
    pure $ True : t'
 stringToBits _         = Nothing

 modulateValue :: Value -> [Bool]
 modulateValue Combat.Data.Nil  = [False, False]
 modulateValue (Pair f s)       = [True , True ] ++ modulateValue f ++ modulateValue s
 modulateValue (Num i)          = modulate i

 demodulateValue :: [Bool] -> Value
 demodulateValue bits = fst $ demodulateV bits
   where
     demodulateV (False: False: res) = (Combat.Data.Nil, res)
     demodulateV (True : True : res) = let
         (l, res1) = demodulateV res
         (r, res2) = demodulateV res1
       in
         (Pair l r, res2)
     demodulateV number = let
         i = demodulate number
         res = drop (length $ modulate i) number
       in
         (Combat.Data.Num i, res)

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
 
 demodulate :: [Bool] -> Integer
 demodulate (s1 : s2 : bs) = sign s1 s2 * num
  where
   width   = length $ takeWhile id bs
   rest    = take (4 * width) $ drop (width + 1) bs
   mayWarn = if length rest /= width * 4 then trace "Missing byted for Integer demodulation, this is likly an error!" else id
   num     = mayWarn $ foldl (\n b -> 2 * n + if b then 1 else 0) 0 rest
 demodulate _              = error "Require at least three bits for demodulation of Integers, got 0 or 1"
 
 sign :: Bool -> Bool -> Integer
 sign True  False = -1
 sign False True  = 1
 sign _     _     = error "Invalid Sign"

 testModulation :: [Bool]
 testModulation =
   map (\n -> n == demodulate (modulate n)) (concat [ [n, -n] | n <- [0 ..] ])
 
