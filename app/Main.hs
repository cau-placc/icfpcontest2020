import           System.Environment
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Exception

import           Text.Parsec
import           Data.Maybe

import AlienNetwork
import Modulation
import           Debug.Trace


main :: IO ()
main = catch (
    do
        args <- getArgs
        let server = head args
            playerKey = read $ args!!1
        putStrLn ("ServerUrl: " ++ server ++ "; PlayerKey: " ++show  playerKey)

        Right result <- join server playerKey
        let Just state = demodulateResponse result
        putStrLn $ "Join:     " <> show state
        combat server playerKey state

    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

combat :: String -> Integer -> GameResponse -> IO ()
combat _      _         (GameResponse Done    _       _ ) = putStrLn "Game Over!"
combat server playerKey InvalidRequest = do
    Right result <- doNothing server playerKey
    let Just state = demodulateResponse result
    putStrLn $ "Nothing:  " <> show state
    combat server playerKey state
combat server playerKey (GameResponse Waiting unknown state) = do
    Right result <- start server playerKey (156,4,10,1)
    let Just state = demodulateResponse result
    putStrLn $ "Start:    " <> show state
    combat server playerKey state
combat server playerKey (GameResponse Running unknown (Just state)) = do
    let Unknown _ role _ _ _ = unknown
    let GameState tick _ ships = state
    let ourCommands = concatMap (createCommandFor role tick ships) ships
    Right result <- command server playerKey ourCommands
    let Just state = demodulateResponse result
    putStrLn $ "Accelerate: " <> show state
    combat server playerKey state
combat server playerKey (GameResponse Running unknown Nothing) = do
    let Unknown _ role _ _ _ = unknown
    Right result <- command server playerKey []
    let Just state = demodulateResponse result
    putStrLn $ "Accelerate: " <> show state
    combat server playerKey state

join :: String ->  Integer -> IO (Either StatusCode ResponseBody)
join server playerKey = let
      body = modulateValue $ toValue [Num 2, Num playerKey, Nil]
    in
      post server "/aliens/send" body

start :: String -> Integer -> ShipConfiguration-> IO (Either StatusCode ResponseBody)
start server playerKey (n1,n2,n3,n4) = let
      body = modulateValue $ toValue [Num 3, Num playerKey, toValue [Num n1, Num n2, Num n3, Num n4]]
    in
      post server "/aliens/send" body

doNothing :: String -> Integer ->  IO (Either StatusCode ResponseBody)
doNothing s p = command s p []

accelerate :: String -> Integer -> ShipId -> Vector -> IO (Either StatusCode ResponseBody)
accelerate server playerKey shipId vector = command server playerKey [Accelerate shipId vector]

createCommandFor :: Role -> Tick -> [(ShipState, [SendCommand])] -> (ShipState, [SendCommand]) -> [SendCommand]
createCommandFor ourrole tick allShips
  (ShipState role idt (Position (Vector x y))
                      (Velocity (Vector xd yd)) _ _ _ _, _)
  | ourrole == role =
    trace ("Predicted: " ++ show predictedPos ++ "; Wanted: " ++ show wantedPos ++
           "Shot at: "   ++ show (tpx, tpy))
    [ Accelerate idt (Vector accX accY)
    , Shoot      idt (Vector tpx  tpy ) 5
    ]
  | otherwise       = []
  where
    (accX, accY) = predictedPos - wantedPos
    wantedPos    = (x, y) + rotate (getGravOffestFor (x, y))
    predictedPos = (x, y) + getGravOffestFor (x, y) + (xd, yd)
    (ShipState _ _ (Position (Vector tx ty))
                   (Velocity (Vector txd tyd)) _ _ _ _, _) =
      getOtherShip role allShips
    (tpx, tpy) = (tx, ty) + getGravOffestFor (tx, ty) + (txd, tyd)

getGravOffestFor :: (Integer, Integer) -> (Integer, Integer)
getGravOffestFor (x,y) = case compare (abs x) (abs y) of
    EQ -> (- signum x, - signum y)
    LT -> (0         , - signum y)
    GT -> (- signum x, 0         )

rotate :: (Integer, Integer) -> (Integer, Integer)
rotate (x, y) = (y, -x)

getOtherShip :: Role -> [(ShipState, [SendCommand])] -> (ShipState, [SendCommand])
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

command :: String -> Integer -> [SendCommand] -> IO (Either StatusCode ResponseBody)
command server playerKey commands = let
      body = modulateValue $ toValue [Num 4, Num playerKey, toValue commands]
    in
      post server "/aliens/send" body

showDemodulated :: String -> String
showDemodulated = show . demodulateValue

demodulateResponse :: String -> Maybe GameResponse
demodulateResponse = fromValue . demodulateValue

type ShipConfiguration = (Integer,Integer,Integer,Integer)
data Status = Waiting |  Running | Done deriving Show

data SendCommand  = Accelerate ShipId Vector
                  | Detonate ShipId
                  | Shoot ShipId Vector Integer
                  deriving Show

data ReceivedCommand = Accelerated Vector
                     | Fired Vector Integer Integer Integer
                     | Other Value
                     deriving Show

data GameResponse = InvalidRequest | GameResponse Status Unknown (Maybe GameState) deriving Show
data Role = Attack | Defence deriving (Show, Eq)
data Unknown = Unknown Integer Role (Integer, Integer, Integer) (Integer, Integer) (Maybe (Integer, Integer, Integer , Integer)) deriving Show
data Tick = Tick Integer deriving Show
data Vector = Vector Integer Integer deriving Show

data ShipId   = ShipId   Integer  deriving Show
data Position = Position Vector   deriving Show
data Velocity = Velocity Vector   deriving Show

data ShipState = ShipState Role ShipId Position Velocity Value Value Value Value  deriving Show
data GameState = GameState Tick Value [(ShipState, [ReceivedCommand])]             deriving Show

class FromValue a where
  fromValue :: Value -> Maybe a

instance FromValue ShipState where
  fromValue v | Just [role, id, position, velocity, x4, x5, x6, x7] <- fromValue v =
      ShipState <$> fromValue role <*> fromValue id <*> fromValue position <*> fromValue velocity <*> fromValue x4 <*> fromValue x5 <*> fromValue x6 <*> fromValue x7
  fromValue _ = Nothing


instance FromValue GameState where
  fromValue v | Just [tick, x3, commands] <- fromValue v =
      GameState <$> fromValue tick <*> fromValue x3 <*> fromValue commands
  fromValue _ = Nothing

instance FromValue Vector where
  fromValue (Pair a b) = Vector <$> fromValue a <*> fromValue b
  fromValue _          = Nothing

instance FromValue Position where
  fromValue v = Position <$> fromValue v

instance FromValue ShipId where
  fromValue v = ShipId <$> fromValue v

instance FromValue Tick where
  fromValue v = Tick <$> fromValue v

instance FromValue Velocity where
  fromValue v = Velocity <$> fromValue v

instance FromValue Integer where
  fromValue (Num i) = pure i
  fromValue _       = Nothing

instance FromValue GameResponse where
  fromValue (Pair (Num 1) (Pair status (Pair unknown (Pair gameState Nil))) ) =
      GameResponse <$> fromValue status <*> fromValue unknown <*> fromValue gameState
  fromValue (Pair (Num 0) Nil) = pure InvalidRequest
  fromValue _ = Nothing

instance FromValue Status where
  fromValue (Num 0) = pure Waiting
  fromValue (Num 1) = pure Running
  fromValue (Num 2) = pure Done
  fromValue _       = Nothing

instance FromValue Role where
  fromValue (Num 0) = pure Attack
  fromValue (Num 1) = pure Defence
  fromValue _       = Nothing

instance FromValue Unknown where
  fromValue v | Just [u1, position, u3, u4, u5] <- fromValue v =
      Unknown <$> fromValue u1 <*> fromValue position <*> fromValue u3 <*> fromValue u4 <*> fromValue u5
  fromValue _      = Nothing

instance (FromValue a) => FromValue [a] where
  fromValue Nil         = pure []
  fromValue (Pair h t)  = (:) <$> fromValue h <*> fromValue t
  fromValue _           = Nothing

instance (FromValue a, FromValue b) => FromValue (a,b) where
  fromValue v | Just [a,b] <- fromValue v = (,) <$> fromValue a <*> fromValue b
  fromValue _ = Nothing

instance (FromValue a, FromValue b, FromValue c) => FromValue (a,b,c) where
  fromValue v | Just [a,b,c] <- fromValue v = (,,) <$> fromValue a <*> fromValue b <*> fromValue c
  fromValue _ = Nothing

instance (FromValue a, FromValue b, FromValue c, FromValue d) => FromValue (a,b,c,d) where
  fromValue v | Just [a,b,c,d] <- fromValue v = (,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d
  fromValue _ = Nothing

instance FromValue SendCommand where
  fromValue v = do
    list <- fromValue v
    case list of
      [Num 0, vector    ] -> Accelerate (ShipId (-1)) <$> fromValue vector
      [Num 1            ] -> pure $ Detonate $ ShipId (-1)
      [Num 2, target, x3] -> Shoot (ShipId (-1)) <$> fromValue target <*> fromValue x3
      _                   -> Nothing


instance FromValue ReceivedCommand where
  fromValue v = do
      case list of
          [Num 0, vector            ] -> Accelerated <$> fromValue vector
          [Num 2, target, x1, x2, x3] -> Fired <$> fromValue target <*> fromValue x1 <*> fromValue x2 <*> fromValue x3
          _                           -> Other v

instance (FromValue a) => FromValue (Maybe a) where
  fromValue v = pure $ fromValue v

instance FromValue Value where
  fromValue = pure


showAlienList :: [String] -> String
showAlienList [] = "nil"
showAlienList [x] = "(" <> x <> ")"
showAlienList (h:t) = let
    (_:t') = showAlienList t
  in
    "(" <> h <> ", " <> t'

class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = id

instance (ToValue a) => ToValue [a] where
  toValue [] = Nil
  toValue (x:h) = Pair (toValue x) $ toValue h

instance (ToValue a, ToValue b) => ToValue (a,b) where
  toValue (a,b) = toValue [toValue a, toValue b]

instance (ToValue a, ToValue b, ToValue c) => ToValue (a,b,c) where
  toValue (a,b,c) = toValue [toValue a, toValue b, toValue c]

instance (ToValue a, ToValue b, ToValue c, ToValue d) => ToValue (a,b,c,d) where
  toValue (a,b,c,d) = toValue [toValue a, toValue b, toValue c, toValue d]

instance (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e) => ToValue (a,b,c,d,e) where
  toValue (a,b,c,d,e) = toValue [toValue a, toValue b, toValue c, toValue d, toValue e]

instance (ToValue a, ToValue b, ToValue c, ToValue d, ToValue e, ToValue f) => ToValue (a,b,c,d,e,f) where
  toValue (a,b,c,d,e,f) = toValue [toValue a, toValue b, toValue c, toValue d, toValue e, toValue f]

instance ToValue Integer where
  toValue = Num

instance ToValue Vector where
  toValue (Vector x y) = Pair (toValue x) $ toValue y

instance ToValue SendCommand where
  toValue (Accelerate shipId vector   ) = toValue (0::Integer, shipId, vector)
  toValue (Detonate   shipId          ) = toValue (1::Integer, shipId)
  toValue (Shoot      shipId target x3) = traceShowId $ toValue (2::Integer, shipId, target, x3)

instance ToValue ShipId where
  toValue (ShipId i) = toValue i

-- data SendCommand = Accelerate ShipId Vector | Detonate ShipId | Shoot ShipId Target Value

data Value =  Nil | Pair Value Value | Num Integer

instance Show Value where
  show Nil          = "nil"
  show (Pair l r)   = case r of
    Nil -> "[" <> show l <> "]"
    Num i -> "(" <> show i <> ")"
    p@(Pair _ _) -> let
        t = show p
      in
        head t : show l <> ", " <> tail t
  show (Num i) = show i

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
