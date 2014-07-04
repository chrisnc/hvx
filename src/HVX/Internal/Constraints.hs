{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module HVX.Internal.Constraints
  ( leq
  , (<=~)
  , geq
  , (>=~)
  , Constraint
  , canConstrain
  , isConstraint
  ) where

import HVX.Primitives
import HVX.Internal.Primitives
import HVX.Internal.DCP

type Constraint = Expr Convex Nonmon

class CanConstrain (v :: Vex) where
    canConstrain :: e (v :: Vex) (m :: Mon) -> e v m
    canConstrain = id

instance CanConstrain Convex
instance CanConstrain Affine

isConstraint :: Constraint -> Constraint
isConstraint = id

-- | lhs <= rhs
-- becomes
-- max(lhs - rhs) <= 0
leq lhs rhs = isConstraint $ EFun Max $ canConstrain $ lhs +~ neg rhs

infix 4 <=~
lhs <=~ rhs = leq lhs rhs

-- | lhs >= rhs
-- becomes
-- max(rhs - lhs) <= 0
geq lhs rhs = isConstraint $ EFun Max $ canConstrain $ rhs +~ neg lhs

infix 4 >=~
lhs >=~ rhs = geq lhs rhs
