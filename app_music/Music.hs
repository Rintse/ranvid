module Music ( generateNotes, toSong ) where

import Eval ( evalExpM )
import Value ( Value (..) )
import Syntax.Grammar.Abs ( Exp (..) )
import Data.Either ( partitionEithers )
import Control.Monad.Except ( MonadError(throwError), runExcept )
import Control.Parallel.Strategies ( using, parListChunk, rdeepseq )
import qualified Haskore.Melody as HM ( c, d, e, f, g, a, b, T, )
import qualified Haskore.Basic.Duration as HD ( qn, T )
import qualified Haskore.Music.GeneralMIDI as HMidi ( 
    fromMelodyNullAttr, Instrument(..)
    )
import Haskore.Music ((+:+), line)
import Debug.Trace (trace)

canvas :: Int -> [Double]
canvas l = map (\c -> (fromIntegral c / fromIntegral l) * 2 - 1) [0..l-1]

type MyNote attr = HD.T -> attr -> HM.T attr

-- TODO: This is only C major scale, make more spicy
validNotes :: [MyNote attr]
validNotes = [n o | o <- octaves, n <- notes ] where
    notes = [ HM.c, HM.d, HM.e, HM.f, HM.g, HM.a, HM.b ]
    octaves = [ 1, 2, 3, 4 ]

-- |The arithmetic operations are defined on [-1, 1]
-- |We need to convert to the required [0, 1] interval to notes
scaleToNote :: Double -> HM.T ()
scaleToNote x = trace (show idx) (validNotes !! idx) HD.qn () where
    idx = round (((x + 1) / 2) * fromIntegral (length validNotes)) - 1

valToNote :: (Double, Double) -> HM.T ()
valToNote (x, y) = scaleToNote y

extractVals :: Either String Value -> Either String (Double, Double)
extractVals (Right (VPair (VVal x) (VVal y))) = Right (x, y)
extractVals _ = error "invalid"

generateNotes :: Exp -> Int -> Int -> Either String (HM.T ())
generateNotes e size p = do
    let applyX cx = App e (DVal cx)
    let calc = map ( extractVals . runExcept . evalExpM . applyX ) (canvas size)

    -- let results = calc
    let results = if p > 1
        then calc `using` parListChunk p rdeepseq
        else calc

    let (errors, note_values) = partitionEithers results
    case errors of
        [] -> Right $ line $ map valToNote note_values
        (err:_) -> throwError $ "Error generating RGBS: " ++ err

toSong melody = HMidi.fromMelodyNullAttr HMidi.AcousticGrandPiano melody
