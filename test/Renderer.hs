module Renderer ( extractPics
                , renderDataAsImage
                , renderDataAsPngTo
                , printDataAsDataUrlPng
                , Img
                ) where

import Codec.Picture
import Control.Monad (join)
import Data.List (nub)
import Data.ByteString.Lazy.Base64
import Data.Text.Lazy hiding (minimum, maximum, count)

import Interpreter
import Interpreter.Data

data Img = Img [(Integer, Integer)]

-- Extracts the pictures from the given data
extractPics :: Data -> MIB [Img]
extractPics (Pic pxs)  = pure $ [Img pxs]
extractPics (Part f p) = do
  res <- tryReduce f p
  case res of
    Part _ es -> join <$> (mapM extractPics =<< mapM runExpr es)
    _         -> pure []
extractPics _          = pure $ []

-- Renders the given data to the provided file path as a PNG image.
renderDataAsPngTo :: String -> [Img] -> IO ()
renderDataAsPngTo fp d = case renderDataAsImage d of
  Just img -> writePng fp img
  Nothing  -> putStrLn "Could not render Data!"

-- Renders the given data to an image if it is a Pic.
renderDataAsImage :: [Img] -> Maybe (Image PixelRGB8)
renderDataAsImage []         = Nothing
renderDataAsImage pxss = Just $ generateImage renderer width height
  where pxs = Prelude.concatMap (\(Img xs) -> xs) pxss
        pxs'            = mapPair fromIntegral <$> pxs
        (minX, minY)    = mapPair minimum $ unzip ((0,0):pxs')
        pxs''           = (\(x, y) -> (x - minX, y - minY)) <$> pxs'
        scale           = 10
        (width, height) = mapPair (* scale) $ mapPair (+ 1) $ mapPair maximum $ unzip ((-minX,-minY):pxs'')
        maxCount        = maximum $ fmap (\x -> Prelude.length $ Prelude.filter (x==) pxs'') $ nub pxs''
        renderer x y    = let pos = (x `div` scale, y `div` scale)
                              isWhite = elem pos pxs''
                              isOrigin = pos == (-minX,-minY)
                              count = Prelude.length $ Prelude.filter (pos==) pxs''
                              c = (fromIntegral $  count * 255 `div` maxCount) :: Pixel8
                              color = case (isWhite, isOrigin) of
                                              (True , False) -> PixelRGB8 c   c c
                                              (False, True ) -> PixelRGB8 255 0 0
                                              (False, False) -> PixelRGB8 0   0 0
                                              (True , True ) -> PixelRGB8 c   c 0
                          in color

printDataAsDataUrlPng :: String -> [Img] -> IO ()
printDataAsDataUrlPng name dat | Just img <- renderDataAsImage dat
                          = let
                              base64 = unpack $ encodeBase64 $ encodePng img
                            in
                              putStrLn $ "<img src='data:image/png;charset=UTF-8;base64," <> base64 <> "'>Image "<> name <>"</img>"
printDataAsDataUrlPng _ _ = putStrLn "Could not render Data"


-- Maps a function over a pair
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
