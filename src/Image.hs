module Image ( rgbsToImg ) where

import Graphics.Image.Interface.Vector
import Graphics.Image ( Pixel(..), RGB, Image, fromListsR )
import Eval (RGBTup)

rgbsToImg :: [[RGBTup]] -> Image VU RGB Double
rgbsToImg rgbs = fromListsR VU $ (map.map) (\(r, g, b) -> PixelRGB r g b) rgbs
