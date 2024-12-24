module Music ( generateNotes, toSong ) where

import Eval ( evalExpM )
import Value ( Value (..) )
import Syntax.Grammar.Abs ( Exp (..) )
import Data.Either ( partitionEithers )
import Control.Monad.Except ( MonadError(throwError), runExcept )
import Control.Parallel.Strategies ( using, parListChunk, rdeepseq )
import qualified Haskore.Melody as HM ( c, d, e, f, g, a, b, T, )
import qualified Haskore.Basic.Duration as HD ( sn, T )
import qualified Haskore.Music.GeneralMIDI as HMidi (
    fromMelodyNullAttr, Instrument(..), T
    )
import Haskore.Music (line, chord)

canvas :: Int -> [Double]
canvas l = map (\c -> (fromIntegral c / fromIntegral l) * 2 - 1) [0..l-1]

type Note = HD.T -> () -> HM.T ()

-- TODO: This is only C major scale, make more spicy
validNotes :: [Note]
validNotes = [n o | o <- octaves, n <- notes ] where
    notes = [ HM.c, HM.d, HM.e, HM.f, HM.g, HM.a, HM.b ]
    octaves = [ 1, 2, 3, 4 ]

-- |The arithmetic operations are defined on [-1, 1]
-- |We need to convert to the required [0, 1] interval to notes
scaleToNote :: Double -> HM.T ()
scaleToNote x = (validNotes !! idx) HD.sn () where
    idx = floor (((x + 1) / 2) * fromIntegral (length validNotes))

getNotes :: Either String Value -> Either String [Double]
getNotes (Right (VL (VVal x))) = Right [x]
getNotes (Right (VL (VL (VPair (VVal x) (VVal y))))) = Right [x, y]
getNotes (Right (VL (VL (VPair (VVal x) (VPair (VVal y) (VVal z)))))) = Right [x, y, z]
getNotes other = error $ "invalid music value: " ++ show other

generateNotes :: Exp -> Int -> Int -> Either String (HM.T ())
generateNotes e size p = do
    let applyX cx = App e (DVal cx)
    let transform = getNotes . runExcept . evalExpM . applyX
    let calc = map transform (canvas size)

    -- let results = calc
    let results = if p > 1
        then calc `using` parListChunk p rdeepseq
        else calc

    let dataToChord = chord . map scaleToNote
    let (errors, note_values) = partitionEithers results
    case errors of
        [] -> Right $ line $ map dataToChord note_values
        (err:_) -> throwError $ "Error generating RGBS: " ++ err

toSong :: HM.T () -> HMidi.T
toSong = HMidi.fromMelodyNullAttr HMidi.AcousticGrandPiano
