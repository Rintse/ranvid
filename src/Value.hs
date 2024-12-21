module Value ( Value (..) ) where
import Syntax.Grammar.Abs (Exp, Ident)

data Value
    = VVal Double
    | VBVal Bool
    | VPair Value Value
    | VL Value
    | VR Value
    | VFun Ident Exp 
    | VError
    deriving (Eq, Ord, Show, Read)
