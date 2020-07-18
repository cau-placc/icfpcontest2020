module Renderer (renderDataAsImage, renderDataAsPngTo) where

import Codec.Picture
import Data.List (maximumBy)
import Data.Ord (comparing)

import Interpreter.Data

-- Renders the given data to the provided file path as a PNG image.
renderDataAsPngTo :: String -> Data -> IO ()
renderDataAsPngTo fp d = case renderDataAsImage d of
  Just img -> writePng fp img
  Nothing  -> putStrLn "Could not render Data!"

-- Renders the given data to an image if it is a Pic.
renderDataAsImage :: Data -> Maybe (Image PixelRGB8)
renderDataAsImage (Pic pxs) = Just $ generateImage renderer width height
  where renderer x y = let isWhite = elem (fromIntegral x, fromIntegral y) pxs
                           color   = if isWhite then 255 else 0
                       in PixelRGB8 (fromIntegral x) (fromIntegral y) color
        width        = fromIntegral $ fst $ maximumBy (comparing fst) pxs
        height       = fromIntegral $ snd $ maximumBy (comparing snd) pxs
renderDataAsImage _         = Nothing
