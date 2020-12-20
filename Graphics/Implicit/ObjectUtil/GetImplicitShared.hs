-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-ticks -dsuppress-module-prefixes -dsuppress-idinfo -dsuppress-unfoldings #-}

module Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared, normalize) where

import {-# SOURCE #-} Graphics.Implicit.Primitives (Object(getImplicit))

import Prelude (flip, (-), (*), (>), (<), (&&), (/), product, abs, (**), fmap, (.), negate, ($), const)

import Graphics.Implicit.Definitions
    ( SymbolicObj2, ℝ2, SymbolicObj3, ℝ3, SharedObj(Empty, Full, Complement, UnionR, IntersectR, DifferenceR, Translate, Scale, Mirror, Shell, Outset, EmbedBoxedObj), ComponentWiseMultable((⋯/)), ℝ, minℝ )

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
    => SharedObj obj (f ℝ)
    -> f ℝ
    -> ℝ
getImplicitShared Empty = const infty
getImplicitShared Full = const $ -infty
getImplicitShared (Complement symbObj) =
  {-# SCC shared_complement  #-} negate . getImplicit symbObj
getImplicitShared (UnionR _ []) =
  {-# SCC shared_empty_union  #-} getImplicitShared @obj Empty
getImplicitShared (UnionR r symbObjs) =
  let objs = fmap getImplicit symbObjs
   in \p -> {-# SCC shared_union  #-} rminimum r $ fmap ($ p) objs
getImplicitShared (IntersectR _ []) =
  {-# SCC shared_empty_intersection  #-} getImplicitShared @obj Full
getImplicitShared (IntersectR r symbObjs) = \p ->
  {-# SCC shared_intersection  #-} rmaximum r $ fmap (`getImplicit` p) symbObjs
getImplicitShared (DifferenceR _ symbObj []) =
  {-# SCC shared_empty_difference  #-} getImplicit symbObj
getImplicitShared (DifferenceR r symbObj symbObjs) =
    let headObj = getImplicit symbObj
        tailObjs = fmap (getImplicitShared . Complement) symbObjs
    in
      {-# SCC shared_difference  #-} \p -> do
        let
          maxTail = rmaximum r
                  $ fmap ($ p) tailObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicitShared (Translate v symbObj) =
  let obj = getImplicit symbObj
   in \p -> {-# SCC shared_translate  #-} obj ({-# SCC shared_translate_subtract #-} p - v)
getImplicitShared (Scale s symbObj) = \p ->
  {-# SCC shared_scale #-} normalize s * getImplicit symbObj (p ⋯/ s)
getImplicitShared (Mirror v symbObj) =
    {-# SCC shared_mirror #-} getImplicit symbObj . reflect v
-- Boundary mods
getImplicitShared (Shell w symbObj) = \p ->
  {-# SCC shared_shell #-} abs (getImplicit symbObj p) - w/2
getImplicitShared (Outset d symbObj) = \p ->
  {-# SCC shared_outset #-} getImplicit symbObj p - d
-- Misc
getImplicitShared (EmbedBoxedObj (obj,_)) = {-# SCC shared_embeded_box #-}obj
{-# INLINABLE getImplicitShared #-}

{-# SPECIALIZE getImplicitShared :: SharedObj SymbolicObj2 ℝ2 -> ℝ2 -> ℝ #-}
{-# SPECIALIZE getImplicitShared :: SharedObj SymbolicObj3 ℝ3 -> ℝ3 -> ℝ #-}

