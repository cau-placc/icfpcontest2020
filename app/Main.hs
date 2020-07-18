import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

main :: IO ()
main = catch (
    do
        args <- getArgs
        putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
        request (args!!0) (args!!1)
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

request :: String -> String -> IO ()
request url body = do
  request' <- parseRequest ("GET " ++ url)
  let request = setRequestBodyLBS (BLU.fromString body) request'
  response <- httpLBS request
  let statuscode = show (getResponseStatusCode response)
  case statuscode of
    "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
    _     -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
