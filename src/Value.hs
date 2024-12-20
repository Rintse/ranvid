module Value ( Value (..) ) where

data Value
    = VIVal Int
    | VDVal Double
    | VBVal Bool
    | VPair Value Value
    | VL Value
    | VR Value
    | VError
    deriving (Eq, Ord, Show, Read)
