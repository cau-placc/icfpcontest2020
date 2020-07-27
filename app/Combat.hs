module Combat (Combat.init) where

import           Prelude hiding (rem)
import           Control.Exception
import           Data.List (stripPrefix)

import           Data.Maybe

import           AlienNetwork
import           Modulation
import           Debug.Trace
import           Combat.Data


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
replace _   _   []  = []
replace pat rep hay@(h:t) = case stripPrefix pat hay of
                              Nothing -> h : replace pat rep t
                              Just rem -> rep <> replace pat rep rem

combat :: Connection -> GameResponse -> IO ()
combat _  (GameResponse Done    _       _ ) = putStrLn "Game Over!"
combat connection InvalidRequest = do
    state <-performAction "Nothing:  " $  doNothing connection
    combat connection state
combat connection (GameResponse Waiting _ _) = do
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
combat connection (GameResponse Running _ Nothing) = do
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
      body = bitsToString $ modulateValue $ toValue [Num 2, Num playerKey, Nil]
    in
      postAlien connection body

start :: Connection -> ShipConfig -> IO (Either StatusCode ResponseBody)
start connection@(Connection _ playerKey _) ShipConfig{fuel = fuel,x2 = x2, x3 = x3, x4 = x4} = let
      body = bitsToString $ modulateValue $ toValue [Num 3, Num playerKey, toValue [Num fuel, Num x2, Num x3, Num x4]]
    in
      postAlien connection body

command :: Connection -> [SendCommand] -> IO (Either StatusCode ResponseBody)
command connection@(Connection _ playerKey _) commands = let
      body = bitsToString $ modulateValue $ toValue [Num 4, Num playerKey, toValue commands]
    in
      postAlien connection body

doNothing :: Connection ->  IO (Either StatusCode ResponseBody)
doNothing connection = command connection []

accelerate :: Connection -> ShipId -> Vector -> IO (Either StatusCode ResponseBody)
accelerate connection shipId vector = command connection [Accelerate shipId vector]

createCommandFor :: Role -> Tick -> [(ShipState, [ReceivedCommand])] -> (ShipState, [ReceivedCommand]) -> [SendCommand]
createCommandFor ourrole tick allShips
  curShip@(ShipState role idt _ _ ShipConfig{x4 = s4} _x2 _x3 _x4, _)
  | ourrole == role = if s4 <= 1 || ourrole == Attack || accX /= 0 || accY /= 0 then -- don't only fork when defending, to conserve fuel to evade and hover
              [Accelerate idt acc
              , Shoot      idt (Vector tpx  tpy ) 64
              ]
            else
              [ Accelerate idt acc
              , Fork       idt (ShipConfig 20 2 5 1) -- TODO base ship config parameter based on our remaining config as they will be taken from us
              ]
  | otherwise       = []
  where
    (ShipState _ _ (Position (Vector tx ty))
                   (Velocity (Vector txd tyd)) _ _ _ _, _) =
      getOtherShip role allShips
    (tpx, tpy) = (tx, ty) + getGravOffestFor (tx, ty) + (txd, tyd)
    acc@(Vector accX accY) = createAccelerationFor ourrole tick allShips curShip

createAccelerationFor :: Role -> Tick -> [(ShipState, [ReceivedCommand])] -> (ShipState, [ReceivedCommand]) -> Vector
createAccelerationFor _ _ _ (ShipState _ idt pos vel _ _ _ _,_)
    = let -- todo evade detonation when defending and close in for detonating when attacking
        (Position (Vector curX curY)) = pos
        (Velocity (Vector curDX curDY)) = vel
        (gX, gY) = getGravOffestFor (curX, curY)
        radius = max 15 $ sqrt $ fromIntegral ((curX^2) + curY^2) :: Float
        maxSpeedComponent = min 5 $ sqrt $ radius / 2
        targetVelocity = case compare (abs curX) (abs curY) of
              EQ -> ((fromIntegral $  signum curX) * maxSpeedComponent
                    ,(fromIntegral $ -signum curY) * maxSpeedComponent
                    )
              LT -> ((fromIntegral $ -signum curY) * maxSpeedComponent
                    ,(fromIntegral $  signum curX) * maxSpeedComponent * (sqrt 2 * (abs $ fromIntegral curX) / radius)
                    )
              GT -> ((fromIntegral $ -signum curY) * maxSpeedComponent * (sqrt 2 * (abs $ fromIntegral curY) / radius)
                    ,(fromIntegral $  signum curX) * maxSpeedComponent
                    )
        currentVelocity = (fromIntegral curDX, fromIntegral curDY)
        velocityDiff = targetVelocity - (currentVelocity + (fromIntegral gX, fromIntegral gY))
        acceleration = -(limit velocityDiff)
        (accX, accY) = trace (show idt <> "\tCurrent Position: "<> show (curX, curY) <> "\tCurrent Velocity: " <> show currentVelocity <> "\tTarget: " <> show targetVelocity <> "\tAcceleration: " <> show acceleration) $ acceleration
      in
        Vector accX accY

limit :: (Float, Float) -> (Integer, Integer)
limit (x,y) = (signum $ round x, signum $ round y)

getGravOffestFor :: (Integer, Integer) -> (Integer, Integer)
getGravOffestFor (x,y) = case compare (abs x) (abs y) of
    EQ -> (- signum x, - signum y)
    LT -> (0         , - signum y)
    GT -> (- signum x, 0         )

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
  fromInteger _ = error "????????"

demodulateResponse :: String -> Maybe GameResponse
demodulateResponse = tryFromValue . demodulateValue . fromJust . stringToBits