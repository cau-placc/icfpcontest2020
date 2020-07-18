import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception

import           AlienNetwork
import           Interpreter

import           Debug.Trace

main :: IO ()
main = undefined

-- main :: IO ()
-- main = do
  -- args <- getArgs
  -- putStrLn ("ServerUrl: " ++ head args ++ "; PlayerKey: " ++ args !! 1)
  -- (printRequestResult =<< post (head args) "" (args !! 1)) `catch` handler
  -- let codedId = modulateToString (Int $ read $ args !! 1)
  -- putStrLn ("CodedId " ++ codedId)
  -- (printRequestResult =<< alienSend (head args) codedId) `catch` handler
  -- (printRequestResult =<< alienSend (head args) ("[0]")) `catch` handler
 -- where
  -- handler :: SomeException -> IO ()
  -- handler ex = putStrLn $ "Request failed with:\n" ++ show ex
-- 