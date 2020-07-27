module Combat.Data (module Combat.Data) where

import qualified Syntax
import           Syntax (AlienExpr)
import           Debug.Trace
import           Interpreter.Data

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
  fromValue :: Value -> a

class TryFromValue a where
  tryFromValue :: Value -> Maybe a

instance TryFromValue ShipState where
  tryFromValue v | Just [role, ident, position, velocity, x4, x5, x6, x7] <- tryFromValue v =
      ShipState <$> tryFromValue role <*> tryFromValue ident <*> tryFromValue position <*> tryFromValue velocity <*> tryFromValue x4 <*> tryFromValue x5 <*> tryFromValue x6 <*> tryFromValue x7
  tryFromValue _ = Nothing


instance TryFromValue ShipConfig where
  tryFromValue v = do
        [fuel, x2, x3, x4] <- tryFromValue v
        pure ShipConfig{fuel = fuel, x2 = x2, x3 = x3, x4 = x4}


instance TryFromValue GameState where
  tryFromValue v | Just [tick, x3, commands] <- tryFromValue v =
      GameState <$> tryFromValue tick <*> tryFromValue x3 <*> tryFromValue commands
  tryFromValue _ = Nothing

instance TryFromValue Vector where
  tryFromValue (Pair a b) = Vector <$> tryFromValue a <*> tryFromValue b
  tryFromValue _          = Nothing

instance TryFromValue Position where
  tryFromValue v = Position <$> tryFromValue v

instance TryFromValue ShipId where
  tryFromValue v = ShipId <$> tryFromValue v

instance TryFromValue Tick where
  tryFromValue v = Tick <$> tryFromValue v

instance TryFromValue Velocity where
  tryFromValue v = Velocity <$> tryFromValue v

instance TryFromValue Integer where
  tryFromValue (Num i) = pure i
  tryFromValue _       = Nothing

instance TryFromValue GameResponse where
  tryFromValue (Pair (Num 1) (Pair status (Pair unknown (Pair gameState Nil))) ) =
      GameResponse <$> tryFromValue status <*> tryFromValue unknown <*> tryFromValue gameState
  tryFromValue (Pair (Num 0) Nil) = pure InvalidRequest
  tryFromValue _ = Nothing

instance TryFromValue Status where
  tryFromValue (Num 0) = pure Waiting
  tryFromValue (Num 1) = pure Running
  tryFromValue (Num 2) = pure Done
  tryFromValue _       = Nothing

instance TryFromValue Role where
  tryFromValue (Num 0) = pure Attack
  tryFromValue (Num 1) = pure Defence
  tryFromValue _       = Nothing

instance TryFromValue Unknown where
  tryFromValue v | Just [u1, position, u3, u4, u5] <- tryFromValue v =
      Unknown <$> tryFromValue u1 <*> tryFromValue position <*> tryFromValue u3 <*> tryFromValue u4 <*> tryFromValue u5
  tryFromValue _      = Nothing

instance (TryFromValue a) => TryFromValue [a] where
  tryFromValue Nil         = pure []
  tryFromValue (Pair h t)  = (:) <$> tryFromValue h <*> tryFromValue t
  tryFromValue _           = Nothing

instance (TryFromValue a, TryFromValue b) => TryFromValue (a,b) where
  tryFromValue v | Just [a,b] <- tryFromValue v = (,) <$> tryFromValue a <*> tryFromValue b
  tryFromValue _ = Nothing

instance (TryFromValue a, TryFromValue b, TryFromValue c) => TryFromValue (a,b,c) where
  tryFromValue v | Just [a,b,c] <- tryFromValue v = (,,) <$> tryFromValue a <*> tryFromValue b <*> tryFromValue c
  tryFromValue _ = Nothing

instance (TryFromValue a, TryFromValue b, TryFromValue c, TryFromValue d) => TryFromValue (a,b,c,d) where
  tryFromValue v | Just [a,b,c,d] <- tryFromValue v = (,,,) <$> tryFromValue a <*> tryFromValue b <*> tryFromValue c <*> tryFromValue d
  tryFromValue _ = Nothing

instance TryFromValue SendCommand where
  tryFromValue v = do
    list <- tryFromValue v
    case list of
      [Num 0, vector    ] -> Accelerate (ShipId (-1)) <$> tryFromValue vector
      [Num 1            ] -> pure $ Detonate $ ShipId (-1)
      [Num 2, target, x3] -> Shoot (ShipId (-1)) <$> tryFromValue target <*> tryFromValue x3
      _                   -> Nothing


instance TryFromValue ReceivedCommand where
  tryFromValue v = do
      list <- tryFromValue v
      case list of
          [Num 0, vector            ] -> Accelerated <$> tryFromValue vector
          [Num 2, target, x1, x2, x3] -> Fired <$> tryFromValue target <*> tryFromValue x1 <*> tryFromValue x2 <*> tryFromValue x3
          _                           -> pure $ Other v

instance (TryFromValue a) => TryFromValue (Maybe a) where
  tryFromValue v = pure $ tryFromValue v

instance FromValue Value where
  fromValue = id 

instance FromValue AlienExpr where
  fromValue (Num i)     = Syntax.Number i
  fromValue Nil         = Syntax.Func Syntax.Nil
  fromValue (Pair x y)  = app Syntax.Cons [fromValue x, fromValue y]

instance FromValue Data where
  fromValue (Num i)     = Int i
  fromValue Nil         = Part Syntax.Nil []
  fromValue (Pair x y)  = Part Syntax.Cons [fromValue x, fromValue y]

instance TryFromValue Value where
  tryFromValue = pure . fromValue

instance TryFromValue AlienExpr where
  tryFromValue = pure . fromValue

instance TryFromValue Data where
  tryFromValue = pure . fromValue

instance TryFromValue a => FromValue (Maybe a) where
  fromValue = tryFromValue

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
