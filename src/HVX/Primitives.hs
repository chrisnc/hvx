{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module HVX.Primitives
-- TODO(mh): Currently primitives that should generate implicit constraints are
-- unsupported. The should be supported shortily. (2014-06-04)
  ( apply
  , hadd
  , (+~)
  , hmul
  , (*~)
  , habs
  , neg
  , hlog
  , hexp
  , logsumexp
  , hmax
  , hmin
  , norm
  , berhu
  , huber
  , quadform
  , powBaseP0
  , powBaseP01
  , powBaseP1
  , powBaseP1InfEven
  , powBaseP1InfNotInt
  ) where

import Numeric.LinearAlgebra hiding (i)
import Numeric.LinearAlgebra.LAPACK

import HVX.Internal.DCP
import HVX.Internal.Matrix
import HVX.Internal.Primitives
import HVX.Internal.Util

apply :: (Vex vf, Mon mf, Vex ve, Mon me, Vex (ApplyVex vf mf ve me), Mon (ApplyMon mf me))
  => Fun vf mf -> Expr ve me -> Expr (ApplyVex vf mf ve me) (ApplyMon mf me)
apply = EFun

hadd :: (Vex v1, Mon m1, Vex v2, Mon m2, Vex (AddVex v1 v2), Mon (AddMon m1 m2))
  => Expr v1 m1 -> Expr v2 m2 -> Expr (AddVex v1 v2) (AddMon m1 m2)
hadd = EAdd

infixl 6 +~
(+~) :: (Vex v1, Mon m1, Vex v2, Mon m2, Vex (AddVex v1 v2), Mon (AddMon m1 m2))
  => Expr v1 m1 -> Expr v2 m2 -> Expr (AddVex v1 v2) (AddMon m1 m2)
(+~) = hadd

-- Constructors that enforce DCP constraints.
hmul :: (Vex v1, Mon m1, Vex (ApplyVex Affine Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Expr Affine Const -> Expr v1 m1 -> Expr (ApplyVex Affine Nonmon v1 m1) (ApplyMon Nonmon m1)
hmul (EConst a) e = apply (Mul a) e
hmul _ _ = error "the left argument of a multiply must be a constant"

infixl 7 *~
(*~) :: (Vex v1, Mon m1, Vex (ApplyVex Affine Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Expr Affine Const -> Expr v1 m1 -> Expr (ApplyVex Affine Nonmon v1 m1) (ApplyMon Nonmon m1)
(*~) (EConst a) e = apply (Mul a) e
(*~) _ _ = error "the left argument of a multiply must be a constant"

habs :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Expr v1 m1 -> Expr (ApplyVex Convex Nonmon v1 m1) (ApplyMon Nonmon m1)
habs = apply Abs

neg :: (Vex v1, Mon m1, Vex (ApplyVex Affine Noninc v1 m1), Mon (ApplyMon Noninc m1))
  => Expr v1 m1 -> Expr (ApplyVex Affine Noninc v1 m1) (ApplyMon Noninc m1)
neg = apply Neg

hlog :: (Vex v1, Mon m1, Vex (ApplyVex Concave Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Expr v1 m1 -> Expr (ApplyVex Concave Nondec v1 m1) (ApplyMon Nondec m1)
hlog = apply Log

hexp :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Expr v1 m1 -> Expr (ApplyVex Convex Nondec v1 m1) (ApplyMon Nondec m1)
hexp = apply Exp

logsumexp :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Expr v1 m1 -> Expr (ApplyVex Convex Nondec v1 m1) (ApplyMon Nondec m1)
logsumexp = apply LogSumExp

hmax :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Expr v1 m1 -> Expr (ApplyVex Convex Nondec v1 m1) (ApplyMon Nondec m1)
hmax = apply Max

hmin :: (Vex v1, Mon m1, Vex (ApplyVex Concave Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Expr v1 m1 -> Expr (ApplyVex Concave Nondec v1 m1) (ApplyMon Nondec m1)
hmin = apply Min

norm :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Convex Nonmon v1 m1) (ApplyMon Nonmon m1)
norm p
  | p == infinity = error "Internal: Infinity norm should become max . abs."
  | 1 <= p = apply (Norm p)
  | otherwise = error "Internal: Norm only supports p >= 1."

berhu :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Convex Nonmon v1 m1) (ApplyMon Nonmon m1)
berhu m
  | 0 < m = apply (Berhu m)
  | otherwise = error "Internal: Berhu only supports m >= 0."

huber :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Convex Nonmon v1 m1) (ApplyMon Nonmon m1)
huber m
  | 0 < m = apply (Huber m)
  | otherwise = error "Internal: Huber only supports m >= 0."

quadform :: (Mon m1, Vex (ApplyVex Convex Nonmon Affine m1), Mon (ApplyMon Nonmon m1))
  => Expr Affine Const -> Expr Affine m1 -> Expr (ApplyVex Convex Nonmon Affine m1) (ApplyMon Nonmon m1)
quadform (EConst a) e
  | rows a == cols a
    && fpequalsMat a (trans a)
    && 0 <= maxElement (eigOnlyS a) = apply (Quadform a) e
  | otherwise = error "Matrices in quadratic forms must be positive semidefinite."
quadform _ _ = error "The matrix sandwitched by the quadratic form must be constant."

powBaseP0 :: (Vex v1, Mon m1, Vex (ApplyVex Affine Const v1 m1), Mon (ApplyMon Const m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Affine Const v1 m1) (ApplyMon Const m1)
powBaseP0 p
  | p `fpequals` 0 = apply PowBaseP0
  | otherwise = error "Internal: PowBaseP0 only supports p == 0."

powBaseP01 :: (Vex v1, Mon m1, Vex (ApplyVex Concave Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Concave Nondec v1 m1) (ApplyMon Nondec m1)
powBaseP01 p
  | 0 < p && p < 1 = apply (PowBaseP01 p)
  | otherwise = error "Internal: PowBaseP01 only supports 0 < p < 1."

powBaseP1 :: (Vex v1, Mon m1, Vex (ApplyVex Affine Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Affine Nondec v1 m1) (ApplyMon Nondec m1)
powBaseP1 p
  | p `fpequals` 1 = apply PowBaseP1
  | otherwise = error "Internal: PowBaseP1 only supports p == 1."

powBaseP1InfEven :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nonmon v1 m1), Mon (ApplyMon Nonmon m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Convex Nonmon v1 m1) (ApplyMon Nonmon m1)
powBaseP1InfEven p
  | 1 < p && isInteger p && even intP = apply (PowBaseP1InfEven intP)
  | otherwise = error "Internal: PowBaseP1InfEven only supports even p > 1."
  where intP = round p :: Integer

powBaseP1InfNotInt :: (Vex v1, Mon m1, Vex (ApplyVex Convex Nondec v1 m1), Mon (ApplyMon Nondec m1))
  => Double -> Expr v1 m1 -> Expr (ApplyVex Convex Nondec v1 m1) (ApplyMon Nondec m1)
powBaseP1InfNotInt p
  | 1 < p && not (isInteger p) = apply (PowBaseP1InfNotInt p)
  | otherwise = error "Internal: PowBaseP1InfNotInt only supports non integral p > 1."
