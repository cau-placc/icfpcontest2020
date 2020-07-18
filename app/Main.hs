import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception

import           Text.Parsec

import AlienNetwork
import Modulation


main :: IO ()
main = catch (
    do
        args <- getArgs
        let server = args!!0
            playerKey = read $ args!!1
        putStrLn ("ServerUrl: " ++ server ++ "; PlayerKey: " ++show  playerKey)

        Right result <- join server playerKey
        putStrLn $ showDemodulated result
        Right result <- start server playerKey (1,2,3,4)
        putStrLn $ showDemodulated result
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

join ::String ->  Integer -> IO (Either StatusCode ResponseBody)
join server playerKey = let
      body = modulateValue $ toValue [Num 2, Num playerKey, Nil]
    in
      post server "/aliens/send" body

showDemodulated :: String -> String
showDemodulated = show . demodulateValue

type ShipConfiguration = (Integer,Integer,Integer,Integer)

type Commands = Value

start :: String -> Integer -> ShipConfiguration-> IO (Either StatusCode ResponseBody)
start server playerKey (n1,n2,n3,n4) = let
      body = modulateValue $ toValue [Num 3, Num playerKey, toValue [Num n1, Num n2, Num n3, Num n4]]
    in
      post server "/aliens/send" body

showAlienList :: [String] -> String
showAlienList [] = "nil"
showAlienList [x] = "(" <> x <> ")"
showAlienList (h:t) = let
    (_:t') = showAlienList t
  in
    "(" <> h <> ", " <> t'

command :: String -> Integer -> [Commands] -> IO (Either StatusCode ResponseBody)
command server playerKey commands = let
      body = modulateValue $ toValue [Num 4, Num playerKey, toValue commands]
    in
      post server "/aliens/send" body

class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = id

instance (ToValue a) => ToValue [a] where
  toValue [] = Nil
  toValue (x:h) = Pair (toValue x) $ toValue h

instance ToValue Integer where
  toValue i = Num i

data Value =  Nil | Pair Value Value | Num Integer

instance Show Value where
  show Nil          = "nil"
  show (Pair l r)   = case r of
    Nil -> "(" <> show l <> ")"
    Num i -> "(" <> show i <> ")"
    p@(Pair _ _) -> "(" <> show l <> ", " <> tail (show p)
  show (Num i) = show i

modulateValue :: Value -> String
modulateValue Nil         = "00"
modulateValue (Pair f s)  = "11" ++ modulateValue f ++ modulateValue s
modulateValue (Num i)     = map (\x -> if x then '1' else '0') $ modulate i

demodulateValue :: String -> Value
demodulateValue string = let
      binary = fmap (\c -> if '0' then False else True) string
    in
      fst $ demodulateV binary
  where
    demodulateV (False: False: res) = (Nil, res)
    demodulateV (True : True : res) = let
        (l, res1) = demodulateV res
        (r, res2) = demodulateV res1
      in
        (Pair l r, res2)
    demodulateV number = let
        i = demodulate number
        res = drop (length $ modulate i) number
      in
        (Num i)