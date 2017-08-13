{-# LANGUAGE DataKinds #-}
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
  , hmulpos
  , (*~+)
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

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix (eigSH')

import HVX.Internal.DCP
import HVX.Internal.Matrix
import HVX.Internal.Primitives
import HVX.Internal.Util

apply :: (ApplyVex vf mf ve me ~ vr, ValidVex vr)
  => Fun vf mf -> Expr ve me -> Expr vr (ApplyMon mf me)
apply = EFun

hadd :: (AddVex v1 v2 ~ v3, ValidVex v3)
  => Expr v1 m1 -> Expr v2 m2 -> Expr v3 (AddMon m1 m2)
hadd = EAdd

infixl 6 +~
(+~) :: (AddVex v1 v2 ~ v3, ValidVex v3)
  => Expr v1 m1 -> Expr v2 m2 -> Expr v3 (AddMon m1 m2)
(+~) = hadd

-- Constructors that enforce DCP constraints.
hmul :: (ApplyVex 'Affine 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Expr 'Affine 'Const -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
hmul (EConst a) e = apply (Mul a) e
hmul _ _ = error "The left argument of a multiply must be a constant"

infixl 7 *~
(*~) :: (ApplyVex 'Affine 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Expr 'Affine 'Const -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
(*~) (EConst a) e = apply (Mul a) e
(*~) _ _ = error "The left argument of a multiply must be a constant"

hmulpos :: (ApplyVex 'Affine 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr 'Affine 'Const -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
hmulpos (EConst a) e
  | 0 <= minElement a = apply (MulPos a) e
  | otherwise = error "The left argument of a left-positive multiply must have non-negative entries"
hmulpos _ _ = error "The left argument of a left-positive multiply must be a constant"

infixl 7 *~+
(*~+) :: (ApplyVex 'Affine 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr 'Affine 'Const -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
(*~+) (EConst a) e
  | 0 <= minElement a = apply (MulPos a) e
  | otherwise = error "The left argument of a left-positive multiply must have non-negative entries"
(*~+) _ _ = error "The left argument of a left-positive multiply must be a constant"

habs :: (ApplyVex 'Convex 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
habs = apply Abs

neg :: (ApplyVex 'Affine 'Noninc v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Noninc m1)
neg = apply Neg

hlog :: (ApplyVex 'Concave 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
hlog = apply Log

hexp :: (ApplyVex 'Convex 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
hexp e = apply Exp e

logsumexp :: (ApplyVex 'Convex 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
logsumexp = apply LogSumExp

hmax :: (ApplyVex 'Convex 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
hmax = apply Max

hmin :: (ApplyVex 'Concave 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
hmin = apply Min

norm :: (ApplyVex 'Convex 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
norm p
  | p == infinity = error "Internal: Infinity norm should become max . abs."
  | 1 <= p = apply (Norm p)
  | otherwise = error "Internal: Norm only supports p >= 1."

berhu :: (ApplyVex 'Convex 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
berhu m
  | 0 < m = apply (Berhu m)
  | otherwise = error "Internal: Berhu only supports m >= 0."

huber :: (ApplyVex 'Convex 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
huber m
  | 0 < m = apply (Huber m)
  | otherwise = error "Internal: Huber only supports m >= 0."

quadform :: (ApplyVex 'Convex 'Nonmon 'Affine m1 ~ v2, ValidVex v2)
  => Expr 'Affine 'Const -> Expr 'Affine m1 -> Expr v2 (ApplyMon 'Nonmon m1)
quadform (EConst a) e
  | rows a == cols a
    && fpequalsMat a (tr a)
    && 0 <= (maxElement.fst.eigSH' $ a) = apply (Quadform a) e -- we have checked that the matrix being passed in is Hermitian, so it's safe to use eigSH'.
  | otherwise = error "Matrices in quadratic forms must be positive semidefinite."
quadform _ _ = error "The matrix sandwitched by the quadratic form must be constant."

powBaseP0 :: (ApplyVex 'Affine 'Const v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Const m1)
powBaseP0 p
  | p `fpequals` 0 = apply PowBaseP0
  | otherwise = error "Internal: PowBaseP0 only supports p == 0."

powBaseP01 :: (ApplyVex 'Concave 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
powBaseP01 p
  | 0 < p && p < 1 = apply (PowBaseP01 p)
  | otherwise = error "Internal: PowBaseP01 only supports 0 < p < 1."

powBaseP1 :: (ApplyVex 'Affine 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
powBaseP1 p
  | p `fpequals` 1 = apply PowBaseP1
  | otherwise = error "Internal: PowBaseP1 only supports p == 1."

powBaseP1InfEven :: (ApplyVex 'Convex 'Nonmon v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nonmon m1)
powBaseP1InfEven p
  | 1 < p && isInteger p && even intP = apply (PowBaseP1InfEven intP)
  | otherwise = error "Internal: PowBaseP1InfEven only supports even p > 1."
  where intP = round p :: Integer

powBaseP1InfNotInt :: (ApplyVex 'Convex 'Nondec v1 m1 ~ v2, ValidVex v2)
  => Double -> Expr v1 m1 -> Expr v2 (ApplyMon 'Nondec m1)
powBaseP1InfNotInt p
  | 1 < p && not (isInteger p) = apply (PowBaseP1InfNotInt p)
  | otherwise = error "Internal: PowBaseP1InfNotInt only supports non integral p > 1."
