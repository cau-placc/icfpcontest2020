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
        let redacted = case  api of
                      Just apiKey -> replace apiKey "<REDACTED>" $ show ex
                      Nothing -> show ex
        in
          putStrLn $ "Unexpected server response:\n" <> redacted


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
    state' <- performAction "Start:    " $ start connection ShipConfig{fuel=300,x2=4,x3=10,x4=2}
    case state' of
        InvalidRequest -> pure ()
        _              -> combat connection state'
combat connection (GameResponse Running unknown (Just state)) = do
    let Unknown _ role _ _ _ = unknown
        GameState tick _ ships = state
        ourCommands = concatMap (createCommandFor role tick ships) ships
    putStrLn $ "Sending:" <> show ourCommands
    state' <- performAction "Commands:   " $ command connection ourCommands
    combat connection state'
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
  curShip@(ShipState role idt (Position (Vector x y))
                      (Velocity (Vector xd yd)) ShipConfig{x4 = s4} x2 x3 x4, _)
  | ourrole == role = if s4 <= 1 || ourrole == Defence then -- don't only fork when defending, to conserve fuel to evade and hover
              ( Shoot      idt (Vector tpx  tpy ) 5
              : acc
              )
            else
              ( Fork       idt (ShipConfig 150 2 5 1) -- TODO base ship config parameter based on our remaining config as they will be taken from us
              : acc
              )
  | otherwise       = []
  where
    (ShipState _ _ (Position (Vector tx ty))
                   (Velocity (Vector txd tyd)) _ _ _ _, _) =
      getOtherShip role allShips
    (tpx, tpy) = (tx, ty) + getGravOffestFor (tx, ty) + (txd, tyd)
    acc = createAccelerationFor ourrole tick allShips curShip

createAccelerationFor :: Role -> Tick -> [(ShipState, [ReceivedCommand])] -> (ShipState, [ReceivedCommand]) -> [SendCommand]
createAccelerationFor ourRole _ _ (ShipState  role idt pos vel conf _ _ _,_)
    | ourRole == role
    = let -- todo evade detonation when defending and close in for detonating when attacking
        (Position (Vector curX curY)) = pos
        (Velocity (Vector curDX curDY)) = vel
        gravity@(gX, gY) = getGravOffestFor (curX, curY)
        radius = max 16 $ sqrt $ fromIntegral ((curX^2) + curY^2)
        maxSpeedComponent = sqrt $ radius / 2
        targetVelocity = case compare (abs curX) (abs curY) of
              EQ -> ((fromIntegral $  signum curX) * maxSpeedComponent
                    ,(fromIntegral $ -signum curY) * maxSpeedComponent
                    )
              LT -> ((fromIntegral $ -signum curY) * maxSpeedComponent
                    ,(fromIntegral $  signum curX) * (fromIntegral $ ceiling $ maxSpeedComponent * (sqrt 2 * (abs $ fromIntegral curX) / radius))
                    )
              GT -> ((fromIntegral $ -signum curY) * (fromIntegral $ ceiling $ maxSpeedComponent * (sqrt 2 * (abs $ fromIntegral curY) / radius))
                    ,(fromIntegral $  signum curX) * maxSpeedComponent
                    )
        currentVelocity = (fromIntegral curDX, fromIntegral curDY)
        velocityDiff = targetVelocity - (currentVelocity  + (fromIntegral gX, fromIntegral gY))
        (accX, accY) = limit velocityDiff
      in
        [Accelerate idt (Vector accX accY)]
    | otherwise = []

rotateF :: (Float,Float) -> Float -> (Float, Float)
rotateF (x,y) a = (x * cos a - y * sin a, x * sin a + y * cos a)

limit :: (Float, Float) -> (Integer, Integer)
limit (x,y) = (signum $ round x, signum $ round y)

scale :: Float -> (Float, Float) -> (Float , Float)
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