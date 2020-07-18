module AlienNetwork where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU

type StatusCode = String
type ResponseBody = String

printRequestResult :: Either StatusCode ResponseBody -> IO ()
printRequestResult requestResult = do
  case requestResult of
      Right body      -> putStrLn ("Server response: " ++ body)
      Left statuscode -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode)

--------------------------------------------------------------------------------
--                              ENDPOINTS                                     --
--------------------------------------------------------------------------------

alienSend :: String -> String -> IO (Either StatusCode String)
alienSend server body = post server "/aliens/send" body

alienReceive :: String -> String -> IO (Either StatusCode String)
alienReceive server responseId = get server ("/aliens/" ++ responseId) ""

-- TODO url encode key
getLogs :: String -> String -> IO (Either StatusCode String)
getLogs server key = get server ("/logs?logKey="++key) ""

--------------------------------------------------------------------------------

post :: String -> String -> String -> IO (Either StatusCode ResponseBody)
post s e b = send s e "POST" b

get :: String -> String -> String -> IO (Either StatusCode ResponseBody)
get s e b = send s e "GET" b

send :: String -> String -> String -> String -> IO (Either StatusCode ResponseBody)
send server endpoint method body = do
    request' <- parseRequest (method ++ " " ++ server ++ endpoint)
    let request = setRequestBodyLBS (BLU.fromString body) request'
    response <- httpLBS request
    let statuscode = show (getResponseStatusCode response)
    case statuscode of
      "200" -> return $ Right $ BLU.toString (getResponseBody response)
      _     -> return $ Left statuscode

