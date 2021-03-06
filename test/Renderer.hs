module Renderer ( extractPics
                , renderDataAsImage
                , renderDataAsPngTo
                , printDataAsDataUrlPng
                ) where

import Codec.Picture
import Control.Monad (join)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.ByteString.Lazy.Base64
import Data.Text.Lazy hiding (minimum, maximum)

import Interpreter
import Interpreter.Data

-- Extracts the pictures from the given data
extractPics :: Data -> MIB [Data]
extractPics (Pic pxs)  = pure $ [(Pic pxs)]
extractPics (Part f p) = do
  res <- tryReduce f p
  case res of
    Part _ es -> join <$> (mapM extractPics =<< mapM runExpr es)
    _         -> pure []
extractPics _          = pure $ []

-- Renders the given data to the provided file path as a PNG image.
renderDataAsPngTo :: String -> Data -> IO ()
renderDataAsPngTo fp d = case renderDataAsImage d of
  Just img -> writePng fp img
  Nothing  -> putStrLn "Could not render Data!"

-- Renders the given data to an image if it is a Pic.
renderDataAsImage :: Data -> Maybe (Image PixelRGB8)
renderDataAsImage (Pic pxs) = Just $ generateImage renderer width height
  where pxs'            = mapPair fromIntegral <$> pxs
        (minX, minY)    = mapPair minimum $ unzip pxs'
        pxs''           = (\(x, y) -> (x - minX, y - minY)) <$> pxs'
        scale           = 10
        (width, height) = mapPair (* scale) $ mapPair (+ 1) $ mapPair maximum $ unzip pxs''
        renderer x y    = let isWhite = elem (x `div` scale, y `div` scale) pxs''
                              color   = if isWhite then 255 else 0
                          in PixelRGB8 color color color
renderDataAsImage _         = Nothing

printDataAsDataUrlPng :: String -> Data -> IO ()
printDataAsDataUrlPng name dat | Just img <- renderDataAsImage dat
                          = let
                              base64 = unpack $ encodeBase64 $ encodePng img
                            in
                              putStrLn $ "<img src='data:image/png;charset=UTF-8;base64," <> base64 <> "'>Image "<> name <>"</img>"
printDataAsDataUrlPng _ _ = putStrLn "Could not render Data"


-- Maps a function over a pair
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
