module Image ( rgbsToImg ) where

import Graphics.Image.Interface.Vector
import Graphics.Image ( Pixel(..), RGB, Image, fromLists )
import Eval (RGBTup)

rgbsToImg :: [[RGBTup]] -> Image VU RGB Double
rgbsToImg rgbs = do  
    let scale x = (x + 1) / 2
    let scaled = (map.map) (\(r, g, b) -> (scale r, scale g, scale b)) rgbs
    fromLists $ (map.map) (\(r, g, b) -> PixelRGB r g b) scaled
