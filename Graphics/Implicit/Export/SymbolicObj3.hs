-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- The purpose of this function is to symbolicaly compute triangle meshes using the symbolic system where possible.
-- Otherwise we coerce it into an implicit function and apply our modified marching cubes algorithm.

module Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh) where

import Prelude(pure, zip, length, filter, (>), ($), null, (<>), foldMap, (.), (<$>))

import Graphics.Implicit.Definitions (currentRounding, GetImplicitContext, ℝ, ℝ3, SymbolicObj3(Shared3), SharedObj(Union), Triangle, TriangleMesh(TriangleMesh))
import Graphics.Implicit.Export.Render (getMesh)
import Graphics.Implicit.Primitives (getBox)
import Graphics.Implicit.MathUtil(box3sWithin)

import Control.Arrow(first, second)

symbolicGetMesh :: ℝ -> GetImplicitContext -> SymbolicObj3 -> TriangleMesh
symbolicGetMesh res ctx inputObj@(Shared3 (Union objs)) = TriangleMesh $
    let
        boxes = getBox <$> objs
        boxedObjs = zip boxes objs

        sepFree :: [((ℝ3, ℝ3), a)] -> ([a], [a])
        sepFree ((box,obj):others) =
            if length (filter (box3sWithin (currentRounding ctx) box) boxes) > 1
            then first  (obj : ) $ sepFree others
            else second (obj : ) $ sepFree others
        sepFree [] = ([],[])

        (dependants, independents) = sepFree boxedObjs
    in if null independents
          then unmesh $ getMesh (pure res) inputObj
          else if null dependants
                  then foldMap (unmesh . symbolicGetMesh res ctx) independents
                  else foldMap (unmesh . symbolicGetMesh res ctx) independents
                       <> unmesh (symbolicGetMesh res ctx $ Shared3 $ Union dependants)

-- | If all that fails, coerce and apply marching cubes :(
symbolicGetMesh res _ obj = getMesh (pure res) obj

unmesh :: TriangleMesh -> [Triangle]
unmesh (TriangleMesh m) = m
