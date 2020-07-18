module Renderer ( extractPics
                , renderDataAsImage
                , renderDataAsPngTo
                ) where

import Codec.Picture
import Control.Monad (join)
import Data.List (maximumBy)
import Data.Ord (comparing)

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
        minX            = minimum $ fst <$> pxs'
        minY            = minimum $ snd <$> pxs'
        pxs''           = (\(x, y) -> (x - minX, y - minY)) <$> pxs'
        (width, height) = maximum pxs''
        renderer x y    = let isWhite = elem (x, y) pxs''
                              color   = if isWhite then 255 else 0
                          in PixelRGB8 (fromIntegral x) (fromIntegral y) color
renderDataAsImage _         = Nothing

-- Maps a function over a pair
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
