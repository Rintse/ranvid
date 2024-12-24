{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.AbsF where

import Syntax.Grammar.Abs

import Data.Functor.Foldable.TH

-- These base functors allow for the use of `recursion-schemes` on the
-- generated Exp trees
makeBaseFunctor ''Exp
makeBaseFunctor ''Type
