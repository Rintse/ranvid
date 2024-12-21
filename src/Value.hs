module Value ( Value (..) ) where

data Value
    = VVal Double
    | VBVal Bool
    | VPair Value Value
    | VL Value
    | VR Value
    | VError
    deriving (Eq, Ord, Show, Read)
