import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("ServerUrl: " ++ head args ++ "; PlayerKey: " ++ args !! 1)
  request (head args) (args !! 1) `catch` handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Request failed with:\n" ++ show ex

request :: String -> String -> IO ()
request url body = do
  request' <- parseRequest ("POST " ++ url)
  let request = setRequestBodyLBS (BLU.fromString body) request'
  response <- httpLBS request
  let statuscode = show (getResponseStatusCode response)
  case statuscode of
    "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
    _     -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode)
