module Main (main) where

import           System.Exit
import           Data.Maybe (fromMaybe)
import           Data.IORef

import           Graphics.Gloss.Interface.IO.Interact
import           Graphics.Gloss.Interface.Environment

import qualified AlienApi
import           Combat.Data
import           Executor
import           Interpreter
import           Parser

data State = State { state :: InteractState
                     , img :: [Img]
                     , history :: Maybe State
                     , scaling :: IORef Float
                     }

main :: IO ()
main = do
  galaxy <- readFile AlienApi.galaxyFile
  let Right galaxyProgram = either (error . show) Right $ parseAlienProg galaxy
  (firstState, initialImage) <- stepGalaxy InteractState{value = Nil, prog = loadProg galaxyProgram} (0,0)

  let
    -- display = InWindow "GalaxyPad" (1920,1080) (0,0) -- TODO fix offset problem in Window mode
    display = FullScreen
    color   = black
  ref <- newIORef 1
  interactIO display color State{state = firstState, img = initialImage, history = Nothing, scaling = ref} draw update callback

draw :: State -> IO Picture
draw State{img = images, scaling = ref} = do
  displayScale <- imageScale images =<< getScreenSize -- TODO only works correctly in fullscreen
  putStrLn $ "Calculated Scale " <> show displayScale
  writeIORef ref displayScale
  pure $ scale displayScale displayScale $ pictures $ imagesToPictures images


imageScale :: [Img] -> (Int, Int) -> IO Float
imageScale images (screenWidth, screenHeight) = let
    (xs, ys)    = unzip $ concatMap (\(Img pixel) -> pixel) images
    width       = 2 * (maximum $ map abs xs) :: Integer
    height      = 2 * (maximum $ map abs ys) :: Integer
    widthScale  = (toInteger screenWidth ) `div` (width  + 1)
    heightScale = (toInteger screenHeight) `div` (height + 1)
  in do
    putStrLn $ "Image  Width: " <> show width       <> " Height: " <> show height
    putStrLn $ "Screen Width: " <> show screenWidth <> " Height: " <> show screenHeight
    putStrLn $ "Scale  Width: " <> show widthScale  <> " Height: " <> show heightScale
    pure $ fromInteger $ minimum [widthScale, heightScale]

imagesToPictures :: [Img] -> [Picture]
imagesToPictures images = fmap (\(Img pixels) -> pictures $ fmap posToPicture pixels) images
  where
    posToPicture (x,y) = color c $ translate (fromIntegral x) (fromIntegral (-y)) $ rectangleSolid 1 1
    c = withAlpha (1.0/fromIntegral  (length images)) white

-- TODO don't do nothing
update :: Event -> State -> IO State
update (EventMotion _                           ) state        = pure state
update (EventResize _                           ) state        = pure state
update (EventKey    (Char       _  ) _      _ _ ) state        = pure state
update (EventKey    (SpecialKey key) kState _ _ ) currentState = case kState of
    Down -> pure currentState
    Up   -> case key of
        KeyEsc       -> exitSuccess
        KeyLeft      -> pure $ fromMaybe currentState $ history currentState -- KeyBackspace did't fire
        key          -> do
            putStrLn $ "Ignoring " <> show key
            pure currentState
update (EventKey    (MouseButton button) kState _modifier (x, y)) currentState =
    case kState of
        Down -> pure currentState -- TODO register down position and only act on up when same
        Up   -> do
            putStrLn $ "Click at " <> show (x,y)
            drawScale <- readIORef $ scaling currentState
            let
               x' = toActualCoord drawScale x
               y' = toActualCoord drawScale (-y)
            putStrLn "Generating next State/Frame"
            -- TODO indicate to the user that we are doing something, when this take a lot of time?
            (newState, newImage) <- stepGalaxy (state currentState) (x', y')
            putStrLn "Done Generating State/Frame"
            pure currentState{state = newState, img = newImage, history = Just currentState}

toActualCoord :: Float -> Float -> Integer
toActualCoord drawScale c = let
    offset = drawScale / 2
    unOffset = c + offset
  in
    floor $ unOffset / drawScale

-- TODO
callback :: Controller -> IO ()
callback _ = pure ()