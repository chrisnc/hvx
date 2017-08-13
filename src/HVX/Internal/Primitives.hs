{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module HVX.Internal.Primitives
  ( Var
  , Fun(..)
  , Expr(..)
  , getFun
  , getJacobian
  , getProperties
  ) where

import Numeric.LinearAlgebra

import HVX.Internal.Matrix
import HVX.Internal.DCP

type Var = String

data Expr vex mon where
  EConst :: Mat -> Expr 'Affine 'Const
  EVar   :: Var -> Expr 'Affine 'Nondec
  EFun   ::
#ifndef DISABLE_EXPR_CXT
      ValidVex vex =>
#endif
      Fun v1 m1 -> Expr v2 m2 -> Expr vex mon
  EAdd   ::
#ifndef DISABLE_EXPR_CXT
      ValidVex vex =>
#endif
    Expr v1 m1 -> Expr v2 m2 -> Expr vex mon

instance Show (Expr vex mon) where
  show (EConst mat) = show mat
  show (EVar   var) = var
  show (EFun   f a) = show f ++ "(" ++ show a ++ ")"
  show (EAdd   a b) = show a ++ " + " ++ show b

getProperties :: (GetVex vex, GetMon mon) => Expr vex mon -> String
getProperties expr = getVex expr ++ " " ++ getMon expr

data Fun (vex :: Vex) (mon :: Mon) where
  Mul                :: Mat -> Fun 'Affine 'Nonmon
  MulPos             :: Mat -> Fun 'Affine 'Nondec
  Abs                :: Fun 'Convex 'Nonmon
  Neg                :: Fun 'Affine 'Noninc
  Log                :: Fun 'Concave 'Nondec
  Exp                :: Fun 'Convex 'Nondec
  LogSumExp          :: Fun 'Convex 'Nondec
  Max                :: Fun 'Convex 'Nondec
  Min                :: Fun 'Concave 'Nondec
  Norm               :: Double -> Fun 'Convex 'Nonmon
  Berhu              :: Double -> Fun 'Convex 'Nonmon
  Huber              :: Double -> Fun 'Convex 'Nonmon
  Quadform           :: Mat -> Fun 'Convex 'Nonmon
  PowBaseP0          :: Fun 'Affine 'Const
  PowBaseP01         :: Double -> Fun 'Concave 'Nondec
  PowBaseP1          :: Fun 'Affine 'Nondec
  PowBaseP1InfEven   :: Integer -> Fun 'Convex 'Nonmon
  PowBaseP1InfNotInt :: Double -> Fun 'Convex 'Nondec

instance Show (Fun vex mon) where
  show (Mul _) = "mul"
  show (MulPos _) = "mulpos"
  show Abs = "abs"
  show Neg = "-"
  show Log = "log"
  show Exp = "exp"
  show LogSumExp = "log_sum_exp"
  show Max = "max"
  show Min = "min"
  show (Norm p) = "norm" ++ show p
  show (Berhu m) = "berhu" ++ show m
  show (Huber m) = "huber" ++ show m
  show (Quadform m) = "quadform" ++ show m
  show PowBaseP0 = "pow0"
  show (PowBaseP01 p) = "pow" ++ show p
  show PowBaseP1 = "pow1"
  show (PowBaseP1InfEven p) = "pow" ++ show p
  show (PowBaseP1InfNotInt p) = "pow" ++ show p

getFun :: Fun vex mon -> Mat -> Mat
getFun (Mul a) x = a <> x
getFun (MulPos a) x = a <> x
getFun Abs x = abs x
getFun Neg x = negate x
getFun Exp x = exp x
getFun Log x
  | anyMat (<= 0) x = error "Cannot take log of number <= 0."
  | otherwise = log x
getFun LogSumExp x = reduceMat (log . sum) $ exp x
getFun Max x = reduceMat maximum x
getFun Min x = reduceMat minimum x
getFun (Norm p) x = scalarMat $ lpnorm p x
getFun (Berhu m) x =
  cmap (\y -> if abs y <= m then abs y else (abs y ** 2 + m ** 2) / (2 * m)) x
getFun (Huber m) x = 
  cmap (\y -> if abs y <= m then abs y ** 2 else 2 * m * abs y - m ** 2) x
getFun (Quadform m) x = tr x <> m <> x
getFun PowBaseP0 x = konst 1.0 ((rows x), (cols x))
getFun (PowBaseP01 p) x
  | allMat (0 <=) x = matrixPow p x
  | otherwise = error "Cannot raise negative number to power p with 0 < p < 1."
getFun PowBaseP1 x = x
getFun (PowBaseP1InfEven p) x = matrixPow (fromIntegral p) x
getFun (PowBaseP1InfNotInt p) x
  | allMat (0 <=) x = matrixPow p x
  | otherwise = error "Cannot raise negative number to a nonintegral power of p for p > 1."

getJacobian :: Fun vex mon -> Mat -> Mat
getJacobian (Mul a) _ = a
getJacobian (MulPos a) _ = a
getJacobian Abs x = diagMat $ signum x
getJacobian Neg x = (-1) * ident (rows x)
getJacobian Log x = diagMat $ 1 / x
getJacobian Exp x = diagMat $ exp x
getJacobian LogSumExp x = tr $ scale (1 / sumElements (exp x)) $ exp x
getJacobian Max x = tr $ ei (rows x) i
  where (i, _) = maxIndex x
getJacobian Min x = tr $ ei (rows x) i
  where (i, _) = minIndex x
getJacobian (Norm p) x = tr $ if x == zeroVec n
  then
    zeroVec n
  else
    scale (1 / denominator) numerator
  where denominator = lpnorm p x ** (p - 1)
        numerator = x * abs x ** (pMat - 2)
        pMat = (1><1) [p]
        n = rows x
getJacobian (Berhu m) x = diagMat $
  cmap (\y -> if abs y <= m then signum y else y / m) x
getJacobian (Huber m) x = diagMat $
  cmap (\y -> if abs y <= m then 2 * y else 2 * m * signum y) x
getJacobian (Quadform m) x = scale 2 $ tr x <> m
getJacobian PowBaseP0 x = konst 0.0 (n, n)
  where n = rows x
getJacobian (PowBaseP01 p) x = scale p $ diagMat $ matrixPow (p - 1) x
getJacobian PowBaseP1 x = ident n
  where n = rows x
getJacobian (PowBaseP1InfEven intP) x = scale p $ diagMat $ matrixPow (p - 1) x
  where p = fromIntegral intP
getJacobian (PowBaseP1InfNotInt p) x = scale p $ diagMat $ matrixPow (p - 1) x
