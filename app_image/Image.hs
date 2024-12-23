module Image ( rgbsToImg, RGBTup, generateRGBs, checkRGBs ) where

import Eval ( evalExpM )
import Value ( Value (..) )
import Syntax.Grammar.Abs ( Exp (..) )
import Graphics.Image.Interface.Vector ( VU(..) )
import Graphics.Image ( Pixel(..), RGB, Image, fromListsR )
import Control.Monad.Except ( MonadError(throwError), runExcept )
import Control.Monad (unless)
import Data.Either ( partitionEithers )
import Data.List ( intercalate )
import Control.Parallel.Strategies ( using, parListChunk, rdeepseq )
import System.Exit ( exitFailure )

type RGBTup = (Double, Double, Double)

showRGBTup :: RGBTup -> String
showRGBTup (r,g,b) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

rgbsToImg :: [[RGBTup]] -> Image VU RGB Double
rgbsToImg rgbs = fromListsR VU $ (map.map) (\(r, g, b) -> PixelRGB r g b) rgbs

-- |A 2d list where each element is a tuple of its coordinates
-- |All the coordinates are normalized to lie within the [-1, 1] range
canvas :: (Int, Int) -> [[(Double, Double)]]
canvas (w, h) = do
    let scaleCoord maxC c = (fromIntegral c / fromIntegral maxC) * 2 - 1
    let widths = map (scaleCoord w) [0..w-1]
    let heights = map (scaleCoord h) [0..h-1]
    map (zip widths . replicate w) heights

-- |The arithmetic operations are defined on [-1, 1]
-- |We need to convert to the required [0, 1] interval for rgb values
scalePixel :: (Double, Double, Double) -> (Double, Double, Double)
scalePixel = f3 (\c -> (c + 1) / 2) where f3 f (a, b, c) = (f a, f b, f c)

-- |Extract the r g and b values
valToRGB :: Either String Value -> Either String RGBTup
valToRGB (Right (VPair (VVal r) (VPair (VVal g) (VVal b)))) =
    Right $ scalePixel (r, g, b)
valToRGB (Right (VPair (VPair (VVal r) (VVal g)) (VVal b))) =
    Right $ scalePixel (r, g, b)
valToRGB (Right other) = Left $ "Did not get a 3-tuple of doubles: " ++ show other
valToRGB (Left e) = Left e

-- |Generate a canvas for expression `e` with size `size`
-- Runs in `p` parallel threads simultaneously
-- Expects all `Rand` nodes to already have been substituted for `DVal`s
generateRGBs :: Exp -> (Int, Int) -> Int -> Either String [[RGBTup]]
generateRGBs e size p = do
    -- Seed the generated function with the X and Y coordinates through `App`s
    let applyXY (cx, cy) = App (App e (DVal cx)) (DVal cy)
    let calcRow = map ( valToRGB . runExcept . evalExpM . applyXY )
    let calcRows = map calcRow (canvas size)

    let results = if p > 1
        then calcRows `using` parListChunk p rdeepseq
        else calcRows

    let (errors, rgbs) = unzip (map partitionEithers results)
    case concat errors of
        [] -> return rgbs
        (err:_) -> throwError $ "Error generating RGBS: " ++ err

-- |Before passing the RGB values to the image library, we make sure the values
-- |are in the valid ranges that it expects
checkRGBs :: [[RGBTup]] -> IO ()
checkRGBs rgbs = do
    let invalids = concatMap (filter isInvalidPixel) rgbs where
            isInvalidPixel (r, g, b) = invalid r || invalid g || invalid b
            invalid c = c < 0 || c > 1

    unless (null invalids) $ do
        putStrLn $ "ERROR: Invalid RGB values: \n  - "
            ++ intercalate "\n  - " (map showRGBTup invalids)
        exitFailure
