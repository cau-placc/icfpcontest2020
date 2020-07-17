import System.Environment
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

import AlienNetwork

import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("ServerUrl: " ++ head args ++ "; PlayerKey: " ++ args !! 1)
  (printRequestResult =<< post (head args) "" (args!!1)) `catch` handler
  let codedId = modulateToString (Int $ read $ args!!1)
  putStrLn ("CodedId" ++ codedId)
  (printRequestResult =<< post (head args) "" codedId) `catch` handler
  (printRequestResult =<< alienSend (head args) (args !! 1)) `catch` handler
  (printRequestResult =<< alienSend (head args) ("[0]")) `catch` handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Request failed with:\n" ++ show ex

data Data = Int Integer | Pair Data Data | Nil

modulateToString :: Data -> String
modulateToString dat = map (\b -> if b then '1' else '0') $ modulateData dat

modulateData :: Data -> [Bool]
modulateData Nil        = [False, False]
modulateData (Pair h t) = True : True : modulateData h ++ modulateData t
modulateData (Int i)    = modulate i

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

stringDemodulate :: String -> Maybe Data
stringDemodulate input = if all (\c -> c == '0' || c == '1') input then
    Just $ demodulateData $ map (\c -> if c == '0' then False else True) input
  else
    Nothing

demodulateData :: [Bool] -> Data
demodulateData dat = let (res ,_) = inner dat in res
  where
    inner (False: False: rem) = (Nil, rem)
    inner (True: True: rem)   = let
                                    (l, rem1) = inner rem
                                    (r, rem2) = inner rem1
                                in
                                  (Pair l r, rem2)
    inner number =
        let
            i = demodulate number
            rem = drop (length $ modulate i) number
        in
            (Int i, rem)


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
