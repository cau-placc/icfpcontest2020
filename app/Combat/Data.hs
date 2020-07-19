module Combat.Data where

import           Debug.Trace

data Status = Waiting
            | Running
            | Done
            deriving Show

data SendCommand  = Accelerate  ShipId Vector
                  | Detonate    ShipId
                  | Shoot       ShipId Vector Integer
                  | Fork        ShipId ShipConfig
                  deriving Show

data ReceivedCommand = Accelerated Vector
                     | Fired Vector Integer Integer Integer
                     | Other Value
                     deriving Show

data Role = Attack
          | Defence
          deriving (Show, Eq)

data GameResponse = InvalidRequest
                  | GameResponse Status Unknown (Maybe GameState)
                  deriving Show

data Unknown = Unknown Integer Role (Integer, Integer, Integer) (Integer, Integer) (Maybe (Integer, Integer, Integer , Integer)) deriving Show

data Vector = Vector Integer Integer deriving Show

data Tick     = Tick     Integer deriving Show
data ShipId   = ShipId   Integer  deriving Show
data Position = Position Vector   deriving Show
data Velocity = Velocity Vector   deriving Show

data ShipConfig = ShipConfig { fuel::Integer, x2 :: Integer, x3::Integer, x4 :: Integer} deriving Show

data ShipState = ShipState Role ShipId Position Velocity ShipConfig Value Value Value  deriving Show
data GameState = GameState Tick Value [(ShipState, [ReceivedCommand])]                 deriving Show


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


class FromValue a where
  fromValue :: Value -> Maybe a

instance FromValue ShipState where
  fromValue v | Just [role, id, position, velocity, x4, x5, x6, x7] <- fromValue v =
      ShipState <$> fromValue role <*> fromValue id <*> fromValue position <*> fromValue velocity <*> fromValue x4 <*> fromValue x5 <*> fromValue x6 <*> fromValue x7
  fromValue _ = Nothing


instance FromValue ShipConfig where
  fromValue v = do
        [fuel, x2, x3, x4] <- fromValue v
        pure ShipConfig{fuel = fuel, x2 = x2, x3 = x3, x4 = x4}


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
      list <- fromValue v
      case list of
          [Num 0, vector            ] -> Accelerated <$> fromValue vector
          [Num 2, target, x1, x2, x3] -> Fired <$> fromValue target <*> fromValue x1 <*> fromValue x2 <*> fromValue x3
          _                           -> pure $ Other v

instance (FromValue a) => FromValue (Maybe a) where
  fromValue v = pure $ fromValue v

instance FromValue Value where
  fromValue = pure


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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
  toValue (Accelerate shipId vector     ) = toValue (0::Integer, shipId, vector)
  toValue (Detonate   shipId            ) = toValue (1::Integer, shipId)
  toValue (Shoot      shipId target x3  ) = traceShowId $ toValue (2::Integer, shipId, target, x3)
  toValue (Fork       shipId shipConfig ) = toValue (3::Integer , shipId , shipConfig)


instance ToValue ShipConfig where
  toValue ShipConfig{fuel = fuel, x2 = x2, x3 = x3, x4 = x4} = toValue (fuel,x2,x3,x4)

instance ToValue ShipId where
  toValue (ShipId i) = toValue i

-- data SendCommand = Accelerate ShipId Vector | Detonate ShipId | Shoot ShipId Target Value

data Value =  Nil | Pair Value Value | Num Integer

instance Show Value where
  show Nil          = "nil"
  show (Pair l r)   = case r of
    Nil -> "[" <> show l <> "]"
    Num i -> "(" <> show l <> ", " <> show i <> ")"
    p@(Pair _ _) -> let
        t = show p
      in
        head t : show l <> ", " <> tail t
  show (Num i) = show i
