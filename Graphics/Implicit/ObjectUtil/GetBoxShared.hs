-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Graphics.Implicit.ObjectUtil.GetBoxShared (VectorStuff(uniformV, elements, corners), intersectBoxes, emptyBox, pointsBox, unionBoxes, outsetBox, getBoxShared, getAndClearRounding, setCurrentRounding, clearRounding) where

import Prelude (snd, Num, (-), (+), pure, (==), max, min, foldr, (/), ($), fmap, (.), not, filter, foldMap, Fractional, Bool, Eq)
import {-# SOURCE #-} Graphics.Implicit.Primitives
    ( Object(getBox') )
import Graphics.Implicit.Definitions
    (currentRounding, GetImplicitContext,  SharedObj(Empty, Full, Complement, Union, Difference, Intersect, Translate, Scale, Mirror, Shell, Outset, EmbedBoxedObj, WithRounding), ComponentWiseMultable((⋯*)), ℝ3, ℝ2, ℝ )
import Graphics.Implicit.MathUtil (infty,  reflect )
import Linear (Metric, V2(V2), V3(V3))
import Data.Foldable (Foldable(toList))
import Control.Applicative (Applicative(liftA2))


------------------------------------------------------------------------------
-- | Ad-hoc methods we need to share code between 2D and 3D. With the exception
-- of 'corners', these are actually all standard methods of other classes,
-- which we don't have access to due to the choice representation for R2 and
-- R3.
--
-- This class is unnecessary if we were to implement #283.
class VectorStuff vec where
  -- | Equivalent to 'Prelude.pure'
  uniformV :: ℝ -> vec
  -- | Equivalent to 'Control.Applicative.liftA2'
  pointwise :: (ℝ -> ℝ -> ℝ) -> vec -> vec -> vec
  -- | Equivalent to 'Data.Foldable.toList'
  elements :: vec -> [ℝ]
  -- | Given a bounding box, produce the points at each corner.
  corners :: (vec, vec) -> [vec]


instance VectorStuff ℝ2 where
  uniformV = pure
  corners (p1@(V2 x1 y1), p2@(V2 x2 y2)) =
    [ p1
    , V2 x1 y2
    , V2 x2 y1
    , p2
    ]
  pointwise = liftA2
  elements = toList
  {-# INLINABLE uniformV #-}
  {-# INLINABLE pointwise #-}
  {-# INLINABLE elements #-}
  {-# INLINABLE corners #-}

instance VectorStuff ℝ3 where
  uniformV = pure
  corners (p1@(V3 x1 y1 z1), p2@(V3 x2 y2 z2)) =
    [ p1
    , V3 x1 y2 z1
    , V3 x2 y2 z1
    , V3 x2 y1 z1
    , V3 x1 y1 z2
    , V3 x2 y1 z2
    , V3 x1 y2 z2
    , p2
    ]
  pointwise = liftA2
  elements  = toList
  {-# INLINABLE uniformV #-}
  {-# INLINABLE pointwise #-}
  {-# INLINABLE elements #-}
  {-# INLINABLE corners #-}


------------------------------------------------------------------------------
-- | Compute the intersection of dimensionality-polymorphic bounding boxes.
intersectBoxes
    :: (VectorStuff a) => [(a, a)] -> (a, a)
intersectBoxes [] = fullBox
intersectBoxes (b : boxes)
  = foldr (biapp (pointwise max) (pointwise min)) b boxes


------------------------------------------------------------------------------
-- | Apply two functions elementwise across pairs. This is the biapplicative
-- operation specialized to pairs.
biapp
    :: (a -> b -> c)
    -> (d -> e -> f)
    -> (a, d)
    -> (b, e)
    -> (c, f)
biapp f g (a1, b1) (a2, b2) = (f a1 a2, g b1 b2)
{-# INLINABLE biapp #-}


-- | An empty box.
emptyBox :: (Applicative f, Num a) => (f a, f a)
emptyBox = (pure 0, pure 0)
{-# INLINABLE emptyBox #-}

-- | A full box.
fullBox :: (VectorStuff vec) => (vec, vec)
fullBox = (uniformV (-infty), uniformV infty)
{-# INLINABLE fullBox #-}

-- | Define a box around all of the given points.
pointsBox :: (Applicative f, Num a, VectorStuff (f a)) => [f a] -> (f a, f a)
pointsBox [] = emptyBox
pointsBox (a : as) = (foldr (pointwise min) a as, foldr (pointwise max) a as)


------------------------------------------------------------------------------
-- | Compute the intersection of dimensionality-polymorphic bounding boxes.
unionBoxes :: (VectorStuff (f a), Applicative f, Eq (f a), Num a, Num (f a)) => ℝ -> [(f a, f a)] -> (f a, f a)
unionBoxes r
  = outsetBox r
  . pointsBox
  . foldMap corners
  . filter (not . isEmpty)

-- | Is a box empty?
isEmpty :: (Eq (f a), Applicative f, Num a, Num (f a)) => (f a, f a) -> Bool
isEmpty (v1, v2) = (v1 - v2) == pure 0

-- | Increase a boxes size by a rounding value.
outsetBox :: (VectorStuff a, Num a) => ℝ -> (a, a) -> (a, a)
outsetBox r (a, b) = (a - uniformV r, b + uniformV r)

-- Get a box around the given object.
getBoxShared
    :: forall obj f a
     .  ( Applicative f, Object obj (f a), VectorStuff (f a), Eq (f a), ComponentWiseMultable (f a), Fractional a, Metric f)
    => GetImplicitContext
    -> SharedObj obj (f a)
    -> (f a, f a)
-- Primitives
getBoxShared _ Empty = emptyBox
getBoxShared _ Full  = fullBox
-- (Rounded) CSG
getBoxShared _ (Complement _) = fullBox
getBoxShared ctx (Union symbObjs) =
  let (r, ctx') = getAndClearRounding ctx
   in unionBoxes r $ fmap (getBox' ctx') symbObjs
getBoxShared ctx (Difference symbObj _)  = getBox' (clearRounding ctx) symbObj
getBoxShared ctx (Intersect symbObjs) =
  intersectBoxes $
    fmap (getBox' $ clearRounding ctx) symbObjs
-- -- Simple transforms
getBoxShared ctx (Translate v symbObj) =
    let (a :: f a, b) = getBox' ctx symbObj
     in (a + v, b + v)
getBoxShared ctx (Scale s symbObj) =
    let
        (a :: f a, b) = getBox' ctx symbObj
        sa = s ⋯* a
        sb = s ⋯* b
     in pointsBox [sa, sb]
getBoxShared ctx (Mirror v symbObj) =
  pointsBox $ fmap (reflect v) $ corners $ getBox' ctx symbObj
-- Boundary mods
getBoxShared ctx (Shell w symbObj) =
    outsetBox (w/2) $ getBox' ctx symbObj
getBoxShared ctx (Outset d symbObj) =
    outsetBox d $ getBox' ctx symbObj
-- Misc
getBoxShared _ (EmbedBoxedObj (_,box)) = box
getBoxShared ctx (WithRounding r obj) = getBox' (setCurrentRounding r ctx) obj

getAndClearRounding :: GetImplicitContext -> (ℝ, GetImplicitContext)
getAndClearRounding ctx = (currentRounding ctx, ctx { currentRounding = 0 })

clearRounding :: GetImplicitContext -> GetImplicitContext
clearRounding = snd . getAndClearRounding

setCurrentRounding :: ℝ -> GetImplicitContext -> GetImplicitContext
setCurrentRounding r ctx = ctx { currentRounding = r }

