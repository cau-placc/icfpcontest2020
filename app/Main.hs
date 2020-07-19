
import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception

import           Text.Parsec
import           Data.Maybe

import AlienNetwork
import Modulation
import           Debug.Trace
import           Combat

main :: IO ()
main = do
    args <- getArgs
    let server = head args
        playerKey = read $ args!!1
    Combat.init $ Connection server playerKey Nothing