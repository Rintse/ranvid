module Value ( Value (..), valToExp ) where

import Syntax.Grammar.Abs (Exp (DVal, InL, InR), Ident)

data Value
    = VVal Double
    | VBVal Bool
    | VPair Value Value
    | VL Value
    | VR Value
    | VFun Ident Exp 
    | VError
    deriving (Eq, Ord, Show, Read)

valToExp :: Value -> Exp
valToExp (VVal d) = DVal d
valToExp (VL a) = InL $ valToExp a
valToExp (VR a) = InR $ valToExp a
valToExp other = error $ "Failed to create exp from value: " ++ show other
