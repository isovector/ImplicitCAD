-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared, normalize) where

import {-# SOURCE #-} Graphics.Implicit.Primitives (Object(getImplicit'))

import Prelude (flip, (-), (*), (>), (<), (&&), (/), product, abs, (**), fmap, (.), negate, ($), const)

import Graphics.Implicit.Definitions
    (currentRounding, GetImplicitContext,  SharedObj(Empty, Full, Complement, Union, Intersect, Difference, Translate, Scale, Mirror, Shell, Outset, EmbedBoxedObj, WithRounding), ComponentWiseMultable((⋯/)), ℝ, minℝ )

import Graphics.Implicit.MathUtil (infty, rmax, rmaximum, rminimum, reflect)

-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import Graphics.Implicit.ObjectUtil.GetBoxShared (VectorStuff(elements, uniformV))

import Linear (Metric(dot))


------------------------------------------------------------------------------
-- | Normalize a dimensionality-polymorphic vector.
normalize
    :: forall f
     . (VectorStuff (f ℝ), Metric f)
    => f ℝ
    -> ℝ
normalize v =
  let all1s = uniformV @(f ℝ) 1
   in abs (product (elements v)) ** (1 / (all1s `dot` all1s))


-- Get a function that describes the surface of the object.
getImplicitShared
    :: forall obj f
     . ( Object obj (f ℝ)
       , VectorStuff (f ℝ)
       , ComponentWiseMultable (f ℝ)
       , Metric f
       )
    => GetImplicitContext
    -> SharedObj obj (f ℝ)
    -> f ℝ
    -> ℝ
getImplicitShared _ Empty = const infty
getImplicitShared _ Full = const $ -infty
getImplicitShared ctx (Complement symbObj) =
  negate . getImplicit' ctx symbObj
getImplicitShared ctx (Union []) =
  getImplicitShared @obj ctx Empty
getImplicitShared ctx (Union symbObjs) = \p ->
  let (r, ctx') = getAndClearRounding ctx
   in rminimum r $ fmap (flip (getImplicit' ctx') p) symbObjs
getImplicitShared ctx (Intersect []) =
  getImplicitShared @obj ctx Full
getImplicitShared ctx (Intersect symbObjs) = \p ->
  let (r, ctx') = getAndClearRounding ctx
   in rmaximum r $ fmap (flip (getImplicit' ctx') p) symbObjs
getImplicitShared ctx (Difference symbObj []) =
  getImplicit' ctx symbObj
getImplicitShared ctx (Difference symbObj symbObjs) =
    let (r, ctx') = getAndClearRounding ctx
        headObj = getImplicit' ctx' symbObj
    in
      \p -> do
        let
          maxTail = rmaximum r
                  $ fmap (flip (getImplicitShared ctx') p . Complement) symbObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicitShared ctx (Translate v symbObj) = \p ->
  getImplicit' ctx symbObj (p - v)
getImplicitShared ctx (Scale s symbObj) = \p ->
  normalize s * getImplicit' ctx symbObj (p ⋯/ s)
getImplicitShared ctx (Mirror v symbObj) =
    getImplicit' ctx symbObj . reflect v
-- Boundary mods
getImplicitShared ctx (Shell w symbObj) = \p ->
  abs (getImplicit' ctx symbObj p) - w/2
getImplicitShared ctx (Outset d symbObj) = \p ->
  getImplicit' ctx symbObj p - d
-- Misc
getImplicitShared _ (EmbedBoxedObj (obj,_)) = obj
getImplicitShared ctx (WithRounding r obj) = getImplicit' (setCurrentRounding r ctx) obj


getAndClearRounding :: GetImplicitContext -> (ℝ, GetImplicitContext)
getAndClearRounding ctx = (currentRounding ctx, ctx { currentRounding = 0 })

setCurrentRounding :: ℝ -> GetImplicitContext -> GetImplicitContext
setCurrentRounding r ctx = ctx { currentRounding = r }

