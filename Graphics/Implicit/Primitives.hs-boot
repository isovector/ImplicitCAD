{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions (GetImplicitContext, SymbolicObj2, SymbolicObj3, SharedObj, ℝ3, ℝ2, ℝ)
import Control.Lens (Prism')
import Prelude (Num)

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class Num vec => Object obj vec | obj -> vec where
    _Shared :: Prism' obj (SharedObj obj vec)
    getBox' :: GetImplicitContext -> obj -> (vec, vec)
    getImplicit' :: GetImplicitContext -> obj -> (vec -> ℝ)


instance Object SymbolicObj2 ℝ2
instance Object SymbolicObj3 ℝ3

getBox :: Object obj vec => obj -> (vec, vec)
getImplicit :: Object obj vec => obj -> vec -> ℝ

