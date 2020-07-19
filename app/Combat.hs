module Combat where

import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception
import           Data.List (stripPrefix)

import           Text.Parsec
import           Data.Maybe

import           AlienNetwork
import           Modulation
import           Debug.Trace
import           Combat.Data


data Connection = Connection String Integer (Maybe String)

init :: Connection -> IO ()
init connection@(Connection server playerKey api) = catch (
      do
        putStrLn ("ServerUrl: " ++ server ++ "; PlayerKey: " ++show  playerKey)
        state <- performAction "Join:     " $ join connection
        combat connection state
    ) handler
    where
      handler :: SomeException -> IO ()
      handler ex =
        let ex' = case  api of
                      Just apiKey -> replace apiKey "<REDACTED>" $ show ex
                      Nothing -> show ex
        in
          putStrLn $ "Unexpected server response:\n" <> ex'


replace :: String -> String -> String -> String
replace pat rep []  = []
replace pat rep hay@(h:t) = case stripPrefix pat hay of
                              Nothing -> h : replace pat rep t
                              Just rem -> rep <> replace pat rep rem

combat :: Connection -> GameResponse -> IO ()
combat _  (GameResponse Done    _       _ ) = putStrLn "Game Over!"
combat connection InvalidRequest = do
    state <-performAction "Nothing:  " $  doNothing connection
    combat connection state
combat connection (GameResponse Waiting unknown state) = do
    state <- performAction "Start:    " $ start connection ShipConfig{fuel=300,x2=4,x3=10,x4=2}
    case state of
        InvalidRequest -> pure ()
        _              -> combat connection state
combat connection (GameResponse Running unknown (Just state)) = do
    let Unknown _ role _ _ _ = unknown
        GameState tick _ ships = state
        ourCommands = concatMap (createCommandFor role tick ships) ships
    state <- performAction "Commands:   " $ command connection ourCommands
    combat connection state
combat connection (GameResponse Running unknown Nothing) = do
    let Unknown _ role _ _ _ = unknown
    state <- performAction "Accelerate: " $ command connection []
    combat connection state

performAction :: String -> IO (Either StatusCode ResponseBody) -> IO GameResponse
performAction name action = do
      Right result <- action
      let Just state = demodulateResponse result
      putStrLn $ name <> show state
      pure state

join :: Connection -> IO (Either StatusCode ResponseBody)
join connection@(Connection _ playerKey _) = let
      body = modulateValue $ toValue [Num 2, Num playerKey, Nil]
    in
      postAlien connection body

start :: Connection -> ShipConfig -> IO (Either StatusCode ResponseBody)
start connection@(Connection _ playerKey _) ShipConfig{fuel = fuel,x2 = x2, x3 = x3, x4 = x4} = let
      body = modulateValue $ toValue [Num 3, Num playerKey, toValue [Num fuel, Num x2, Num x3, Num x4]]
    in
      postAlien connection body

command :: Connection -> [SendCommand] -> IO (Either StatusCode ResponseBody)
command connection@(Connection _ playerKey _) commands = let
      body = modulateValue $ toValue [Num 4, Num playerKey, toValue commands]
    in
      postAlien connection body

postAlien :: Connection -> String -> IO (Either StatusCode ResponseBody)
postAlien (Connection server playerKey maybeKey) body = let
    querry = case maybeKey of
        Nothing -> ""
        Just k -> "?apiKey=" <> k
  in
    post server ("/aliens/send" <> querry) body

doNothing :: Connection ->  IO (Either StatusCode ResponseBody)
doNothing connection = command connection []

accelerate :: Connection -> ShipId -> Vector -> IO (Either StatusCode ResponseBody)
accelerate connection shipId vector = command connection [Accelerate shipId vector]

createCommandFor :: Role -> Tick -> [(ShipState, [ReceivedCommand])] -> (ShipState, [ReceivedCommand]) -> [SendCommand]
createCommandFor ourrole tick allShips
  (ShipState role idt (Position (Vector x y))
                      (Velocity (Vector xd yd)) ShipConfig{x4 = s4} x2 x3 x4, _)
  | ourrole == role =
    trace ("Predicted: " ++ show predictedPos ++ "; Wanted: " ++ show wantedPos ++
           "Shot at: "   ++ show (tpx, tpy)) $
            if s4 <= 1 then
              [ Accelerate idt (Vector accX accY)
              , Shoot      idt (Vector tpx  tpy ) 5
              ]
            else
              [ Accelerate idt (Vector accX accY)
              , Fork       idt [toValue ShipConfig{fuel = 150,x2=5,x3=2,x4=1}]
              ]
  | otherwise       = []
  where
    (accX, accY) = predictedPos - wantedPos
    wantedPos    = (x, y) + (rotate (getGravOffestFor (x, y)))
    predictedPos = (x, y) + getGravOffestFor (x, y) + (xd, yd)
    (ShipState _ _ (Position (Vector tx ty))
                   (Velocity (Vector txd tyd)) _ _ _ _, _) =
      getOtherShip role allShips
    (tpx, tpy) = (tx, ty) + getGravOffestFor (tx, ty) + (txd, tyd)


scale :: Integer -> (Integer, Integer) -> (Integer , Integer)
scale r (x,y) = (x*r, y*r)

getGravOffestFor :: (Integer, Integer) -> (Integer, Integer)
getGravOffestFor (x,y) = case compare (abs x) (abs y) of
    EQ -> (- signum x, - signum y)
    LT -> (0         , - signum y)
    GT -> (- signum x, 0         )

rotate :: (Integer, Integer) -> (Integer, Integer)
rotate (x, y) = (y, -x)

getOtherShip :: Role -> [(ShipState, [ReceivedCommand])] -> (ShipState, [ReceivedCommand])
getOtherShip _ [] = error "Ded."
getOtherShip ourrole (x@(ShipState role _ _ _ _ _ _ _, _):xs)
  | role /= ourrole = x
  | otherwise       = getOtherShip ourrole xs

instance (Num a, Num b) => Num (a, b) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  negate (x, y) = (-x, -y)
  fromInteger n = error "????????"


showDemodulated :: String -> String
showDemodulated = show . demodulateValue

demodulateResponse :: String -> Maybe GameResponse
demodulateResponse = fromValue . demodulateValue

showAlienList :: [String] -> String
showAlienList [] = "nil"
showAlienList [x] = "(" <> x <> ")"
showAlienList (h:t) = let
    (_:t') = showAlienList t
  in
    "(" <> h <> ", " <> t'

modulateValue :: Value -> String
modulateValue Nil         = "00"
modulateValue (Pair f s)  = "11" ++ modulateValue f ++ modulateValue s
modulateValue (Num i)     = map (\x -> if x then '1' else '0') $ modulate i

demodulateValue :: String -> Value
demodulateValue string = fst (demodulateV (fmap (/= '0') string))
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
        (Num i, res)