{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module HVX.Internal.Solvers
  ( constStep
  , decNonSumStep
  , subgradMinimize
  , subgradMaximize
  , subgradLoop
  , ellipsoidMinimize
  , ellipsoidMaximize
  , ellipsoidLoop
  ) where

import Data.List
import Data.Ord
import Data.Maybe
import Numeric.LinearAlgebra
import Control.DeepSeq (deepseq)

import HVX.Primitives (neg)
import HVX.Internal.Constraints
import HVX.Internal.DCP
import HVX.Internal.Matrix
import HVX.Internal.Primitives
import HVX.Internal.SymbolicSubgrad
import HVX.Internal.Util

class ValidVex vex => CanMinimize (vex :: Vex)
instance CanMinimize 'Affine
instance CanMinimize 'Convex

-- | Constant step size.
constStep :: Double -> Int -> Double
constStep alpha _ = alpha

-- | 1/n step size.
decNonSumStep :: Double -> Int -> Double
decNonSumStep alpha itr = alpha / fromIntegral itr

-- | Use subgradient method to solve a minimization problem.
subgradMinimize :: CanMinimize vex =>
     Expr vex mon     -- ^ Objective to minimize.
  -> [Constraint]     -- ^ Constraints on minimization problem.
  -> (Int -> Double)  -- ^ Stepsize function.
  -> Int              -- ^ Number of iterations to run.
  -> Vars             -- ^ Variables and their starting values.
  -> (Vars, Double)   -- ^ Optimal variable values and optimal objective value.
subgradMinimize = subgradLoop 1

-- | Use subgradient method to solve a maximization problem.
subgradMaximize :: CanMinimize (ApplyVex 'Affine 'Noninc vex mon) =>
     Expr vex mon     -- ^ Objective to maximize.
  -> [Constraint]     -- ^ Constraints on maximization problem.
  -> (Int -> Double)  -- ^ Stepsize function.
  -> Int              -- ^ Number of iterations to run.
  -> Vars             -- ^ Variables and their starting values.
  -> (Vars, Double)   -- ^ Optimal variable values and optimal objective value.
subgradMaximize objective constraints stepFun maxItr vars = (fst result, negate $ snd result)
  where result = subgradLoop 1 (neg objective) constraints stepFun maxItr vars

-- | Subgradient solver that minimizes an expression subject to constraints.
subgradLoop :: Int -> Expr vex mon -> [Constraint] -> (Int -> Double) -> Int -> Vars
  -> (Vars, Double)
subgradLoop itr objective constraints stepFun maxItr vars =
  if itr >= maxItr || vars == varsNext
    then (vars, evaluate objective vars `atIndex` (0,0))
    else subgradLoop (itr + 1) objective constraints stepFun maxItr varsNext
      where varsNext = deepseq vars $ map (updateWithSubgrad objective constraints (stepFun itr) vars) vars

updateWithSubgrad :: Expr vex mon -> [Constraint] -> Double -> Vars -> (Var, Mat) -> (Var, Mat)
updateWithSubgrad objective constraints stepSize vars (varname, val) =
  if any isNaN (toList $ flatten g)
    then (varname, val)
    else (varname, val - scale stepSize g)
      where g = fromMaybe (tr $ jacobianWrtVar objective vars varname) $
                          getConstraintSubgrad constraints vars varname

type Soid = (Var, Mat, Mat)

-- | Use ellipsoid method to solve a minimization problem.
ellipsoidMinimize :: CanMinimize vex =>
     Expr vex mon            -- ^ Objective to minimize.
  -> [Constraint]            -- ^ Constraints on minimization problem.
  -> [(Var,Int)]             -- ^ Variable names and their sizes.
  -> Double                  -- ^ Tolerance.
  -> Double                  -- ^ Radius to seed initial sphere with.
  -> (Vars, Double, Double)  -- ^  Optimal variable values, lower bound, and upper bound.
ellipsoidMinimize objective constraints varsizes tol radius =
  ellipsoidLoop objective constraints tol soids (-infinity) infinity
    where soids = map (\(name,n) -> (name, zeroVec n, scale radius $ ident n)) varsizes

-- | Use ellipsoid method to solve a maximization problem.
ellipsoidMaximize :: CanMinimize (ApplyVex 'Affine 'Noninc vex mon) =>
     Expr vex mon            -- ^ Objective to maximize.
  -> [Constraint]            -- ^ Constraints on maximization problem.
  -> [(Var,Int)]             -- ^ Variable names and their sizes.
  -> Double                  -- ^ Tolerance.
  -> Double                  -- ^ Radius to seed initial sphere with.
  -> (Vars, Double, Double)  -- ^  Optimal variable values, lower bound, and upper bound.
ellipsoidMaximize objective constraints varsizes tol radius = (optvars, negate ubound, negate lbound)
  where (optvars, lbound, ubound) = ellipsoidLoop (neg objective) constraints tol soids (-infinity) infinity
        soids = map (\(name,n) -> (name, zeroVec n, scale radius $ ident n)) varsizes

-- | Ellipsoid method solver that minimizes an expression subject to constraints.
ellipsoidLoop :: Expr vex mon -> [Constraint] -> Double -> [Soid] -> Double -> Double
  -> (Vars, Double, Double)
ellipsoidLoop objective constraints tol soids lbound ubound =
  if ubound - lbound < tol || sqrtgpgsum == 0.0
    then (centers, nlbound, nubound)
    else ellipsoidLoop objective constraints tol nsoids nlbound nubound
      where updates = deepseq soids $ map (updateEllipsoid objective constraints centers) soids
            nsoids = map fst updates
            sqrtgpgsum = sqrt . sum . map snd $ updates
            objval = evaluate objective centers `atIndex` (0,0)
            nlbound = max lbound $ objval - sqrtgpgsum
            nubound = min ubound objval
            centers = map (\(name, val, _) -> (name, val)) soids

updateEllipsoid :: Expr vex mon -> [Constraint] -> Vars -> Soid -> (Soid, Double)
updateEllipsoid objective constraints vars soid =
  if gpg == 0.0
    then (soid, objgpg)
    else (nsoid, objgpg)
      where objg = tr $ jacobianWrtVar objective vars varname
            g = fromMaybe objg $ getConstraintSubgrad constraints vars varname
            n = fromIntegral $ rows val
            gpg = (`atIndex` (0,0)) $ tr g <> p <> g
            objgpg = (`atIndex` (0,0)) $ tr objg <> p <> objg
            gtilde = scale (1 / sqrt gpg) g
            nval = val - scale (1 / (n + 1)) (p <> gtilde)
            np = scale (n**2 / (n**2 - 1)) $
              p - scale (2/(n+1)) (p <> (gtilde <> tr gtilde) <> p)
            (varname, val, p) = soid
            nsoid = (varname, nval, np)

-- | Get the subgradient of the maximally violated constraint if there is a
-- violation. Returns nothing if all constraints are satisfied
getConstraintSubgrad :: [Expr vex mon] -> Vars -> String -> Maybe Mat
getConstraintSubgrad constraints vars var =
  if null constraints || maxVal < 0.0
    then Nothing
    else Just cg
      where cg = tr $ jacobianWrtVar maxConstraint vars var
            (maxConstraint, maxVal) = maximumBy (comparing snd) constraintViolations
            constraintViolations = map (\c -> (c, evaluate c vars `atIndex` (0,0))) constraints
