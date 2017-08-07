{-# LANGUAGE FlexibleInstances #-}

module HVX.SymbolicSubgradSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding ((><))
import Numeric.LinearAlgebra

import HVX.Primitives
import HVX.Internal.Matrix
import HVX.Internal.Primitives
import HVX.Internal.TestUtil
import HVX.Internal.SymbolicSubgrad

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "SymbolicSubgrad" $ do
  evaluateSpec
  jacobianWrtVarSpec

evaluateSpec :: Spec
evaluateSpec = describe "evaluate" $ do

  describe "log/exp" $
    it "log(exp(x)) = x" $ property $
      forAll smallVectors $
        \x -> fpequalsMat
          (evaluate (EVar "x") [("x", x)])
          (evaluate (EFun Log (EFun Exp (EVar "x"))) [("x", x)])

  describe "log_sum_exp" $
    it "log_sum_exp(x) = log(ones'*exp(x))" $ property $
      forAll smallVectors $
        \x -> let n = rows x in
          fpequalsMat
            (evaluate (EFun LogSumExp (EVar "x")) [("x", x)])
            (evaluate
              (EFun Log
                (EFun (Mul (konst 1.0 (1, n)))
                  (EFun Exp (EVar "x"))))
              [("x", x)])

  describe "max/min" $
    it "max(x) = -min(-x)" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (evaluate (EFun Max (EVar "x")) [("x", x)])
          (evaluate (EFun Neg (EFun Min (EFun Neg (EVar "x")))) [("x", x)])

  describe "lpnorm" $ do
    it "lpnorm(x, 1) = sum(abs(x))" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (evaluate
            (norm 1 (EVar "x"))
            [("x", x)])
          (evaluate
            (EFun (Mul (konst 1.0 (1, (rows x)))) (habs (EVar "x")))
            [("x", x)])
    it "lpnorm(x, 2)^2 = x^T * x" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (tr' x <> x)
          (evaluate
            (EFun (PowBaseP1InfEven 2) (norm 2 (EVar "x")))
            [("x", x)])

  describe "quadform" $
    it "quadform(I, x) = x^T x" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (evaluate
            (EFun (Quadform (ident $ rows x)) (EVar "x"))
            [("x", x)])
          (tr' x <> x)

  describe "pow (base variable)" $ do
    it "(x^a)^(1/a) = x (a non integral, a > 1)" $ property $
      forAll (pairsOf positiveVectors smallNonIntPowersGreaterThanOne) $
        \(x, p) -> fpequalsMat
          (evaluate (EVar "x") [("x", x)])
          (evaluate
            (EFun (PowBaseP1InfNotInt p) (EFun (PowBaseP01 (1.0/p)) (EVar "x")))
            [("x", x)])
    it "(x^a)^(1/a) = x (a even, a > 1)" $ property $
      forAll (pairsOf positiveVectors smallEvenPowersGreaterThanOne) $
        \(x, p) -> fpequalsMat
          (evaluate (EVar "x") [("x", x)])
          (evaluate
            (EFun (PowBaseP1InfEven (round p :: Integer)) (EFun (PowBaseP01 (1.0/p)) (EVar "x")))
            [("x", x)])

jacobianWrtVarSpec :: Spec
jacobianWrtVarSpec = describe "jacobianWrtVar" $ do

  it "works with respect to a variable that doesn't occur in Expr." $
    forAll (pairsOf vectors vectors) $
      \(x, y) -> fpequalsMat
        (jacobianWrtVar
          (EVar "x")
          [("x", x), ("y", y)]
          "y")
        (konst 0.0 ((rows x), (rows y)))

  describe "log/exp" $
    it "log(exp(x)) - x has zero jacobian" $ property $
      forAll smallVectors $
        \x -> fpequalsMat
          (jacobianWrtVar
            (EAdd (EFun Log (EFun Exp (EVar "x"))) (EFun Neg (EVar "x")))
            [("x", x)]
            "x")
          (zeroMat $ rows x)

  describe "log_sum_exp" $
    it "jacobian log_sum_exp(x) = jacobian log(ones'*exp(x))" $ property $
      forAll smallVectors $
        \x -> fpequalsMat
          (jacobianWrtVar (EFun LogSumExp (EVar "x")) [("x", x)] "x")
          (jacobianWrtVar
            (EFun Log
              (EFun (Mul (konst 1.0 (1, (rows x))))
                (EFun Exp (EVar "x"))))
            [("x", x)]
            "x")

  describe "max/min" $
    it "max(x) + min(-x) has zero jacobian" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (jacobianWrtVar
            (EAdd (EFun Max (EVar "x")) (EFun Min (EFun Neg (EVar "x"))))
            [("x", x)]
            "x")
          (tr' $ zeroVec $ rows x)

  describe "lpnorm" $ do
    it "jacobian lpnorm(x, 1) = jacobian sum(abs(x))" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (jacobianWrtVar
            (norm 1 (EVar "x"))
            [("x", x)]
            "x")
          (jacobianWrtVar
            (EFun (Mul (konst 1.0 (1, (rows x)))) (habs (EVar "x")))
            [("x", x)]
            "x")
    it "jacobian lpnorm(0, 2) = 0" $ property $
      \(Positive n) -> fpequalsMat
        (jacobianWrtVar (norm 2 (EVar "x")) [("x", zeroVec n)] "x")
        (tr' $ zeroVec n)

  describe "quadform" $
    it "jacobian quadform(I, x) = jacobian lpnorm(x, 2)^2" $ property $
      forAll vectors $
        \x -> fpequalsMat
          (jacobianWrtVar (EFun (Quadform (ident $ rows x)) (EVar "x")) [("x", x)] "x")
          (jacobianWrtVar (EFun (PowBaseP1InfEven 2) (norm 2 (EVar "x"))) [("x", x)] "x")

  describe "pow (base variable)" $ do
    it "(x^a)^(1/a) - x has zero jacobian (a non integral, a > 1)" $ property $
      forAll (pairsOf positiveVectors smallNonIntPowersGreaterThanOne) $
        \(x, p) -> fpequalsMat
          (jacobianWrtVar
            (EAdd
              (EFun (PowBaseP1InfNotInt p) (EFun (PowBaseP01 (1.0/p)) (EVar "x")))
              (EFun Neg (EVar "x")))
            [("x", x)]
            "x")
          (zeroMat $ rows x)
    it "(x^a)^(1/a) - x has zero jacobian (a even, a > 1)" $ property $
      forAll (pairsOf positiveVectors smallEvenPowersGreaterThanOne) $
        \(x, p) -> fpequalsMat
          (jacobianWrtVar
            (EAdd
              (EFun (PowBaseP1InfEven (round p :: Integer)) (EFun (PowBaseP01 (1.0/p)) (EVar "x")))
              (neg (EVar "x")))
            [("x", x)]
            "x")
          (zeroMat $ rows x)
