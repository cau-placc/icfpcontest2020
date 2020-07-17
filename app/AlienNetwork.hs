module AlienNetwork

import Network.HTTP.Simple

request :: String -> String -> String -> IO ()
request url endpoint body = do
  request' <- parseRequest ("POST " ++ url ++ endpoint)
  let request = setRequestBodyLBS (BLU.fromString body) request'
  response <- httpLBS request
  let statuscode = show (getResponseStatusCode response)
  case statuscode of
    "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
    _     -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode)


alienSend :: String -> String -> IO ()
alienSend server body = request server "/aliens/send" body