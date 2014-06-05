{-# LANGUAGE GADTs #-}

module HVX
  ( Var
  , Vars
  , Vex
  , Affine
  , Convex
  , Concave
  , Mon
  , Const
  , Nondec
  , Noninc
  , Nonmon
  -- Constructors for supported primitives.
  , Expr(EConst, EVar)
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
  -- Evaluating expressions and their subgradients.
  , evaluate
  , jacobianWrtVar
  -- Constructors for constraints.
  , leq
  , (<=~)
  , geq
  , (>=~)
  , Constraint
  -- Solvers and step size functions.
  , subgradMinimize
  , subgradMaximize
  , ellipsoidMinimize
  , ellipsoidMaximize
  , decNonSumStep
  , constStep
  ) where

import HVX.Primitives
import HVX.Internal.DCP
import HVX.Internal.Constraints
import HVX.Internal.Primitives
import HVX.Internal.Solvers
import HVX.Internal.SymbolicSubgrad
