module Main (main) where

import           System.Environment
import           Combat
import           AlienNetwork (Connection(..))

main :: IO ()
main = do
    args <- getArgs
    let server = head args
        playerKey = read $ args!!1 :: Integer
    Combat.init $ Connection server playerKey Nothing