import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception

import AlienNetwork

main :: IO ()
main = catch (
    do
        args <- getArgs
        let server = args!!0
            playerKey = args!!1
        putStrLn ("ServerUrl: " ++ server ++ "; PlayerKey: " ++ playerKey)

        Right result <- join server playerKey
        putStrLn result
        Right result <- start server playerKey (1,2,3,4)
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

join ::String ->  String -> IO (Maybe JoinResult)
join server playerKey = let
      body = "(2, " <> playerKey <> ", nil)"
    in
      post server "/aliens/send"



type ShipConfiguratin = (Integer,Integer,Integer,Integer)

start :: String -> String -> ShipConfiguration-> IO (Maybe StartResult)
start server playerKey ship = let
      body = "(3," <> playerKey <> "," <> show ship <> ")"
    in
      post server "/aliens/send"