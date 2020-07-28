module AlienNetwork (postAlien, Connection(..), ResponseBody, StatusCode) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU

type StatusCode = String
type ResponseBody = String


data Connection = Connection String Integer (Maybe String)

postAlien :: Connection -> String -> IO (Either StatusCode ResponseBody)
postAlien (Connection server _ maybeKey) body = let
    querry = case maybeKey of
        Nothing -> ""
        Just k -> "?apiKey=" <> k
  in
    send server ("/aliens/send" <> querry) "POST" body

send :: String -> String -> String -> String -> IO (Either StatusCode ResponseBody)
send server endpoint method body = do
    request' <- parseRequest (method ++ " " ++ server ++ endpoint)
    let request = setRequestBodyLBS (BLU.fromString body) request'
    response <- httpLBS request
    let statuscode = show (getResponseStatusCode response)
    case statuscode of
      "200" -> return $ Right $ BLU.toString (getResponseBody response)
      _     -> return $ Left statuscode

