{-# LANGUAGE GADTs #-}

module HVX.Internal.SymbolicSubgrad where

import Numeric.LinearAlgebra hiding (i)
import Numeric.LinearAlgebra.Util

import HVX.Internal.Matrix
import HVX.Internal.Primitives

type Vars = [(Var, Mat)]

evaluate :: Expr vex mon -> Vars -> Mat
evaluate (EConst c) _ = c
evaluate (EVar var) vars
    | Just val <- lookup var vars = val
    | otherwise = error $ "Variable " ++ show var ++ " not set."
evaluate (EFun f e) vars = getFun f (evaluate e vars)
evaluate (EAdd l r) vars = evaluate l vars + evaluate r vars

jacobianWrtVar :: Expr vex mon  -- ^ Expression whose jacobian to take.
  -> Vars                       -- ^ Variables appearing in the expression.
  -> Var                        -- ^ Variable to take jacobian with respect to.
  -> Mat                        -- ^ The jacobian.
jacobianWrtVar (EConst c) vars wrtVar
    | Just wrtVal <- lookup wrtVar vars = zeros (rows c) (rows wrtVal)
    | otherwise = error $ "Variable " ++ show wrtVar ++ " not set."
jacobianWrtVar (EVar var) vars wrtVar
    | var == wrtVar, Just val <- lookup var vars = ident $ rows val
    | Just val <- lookup var vars, Just wrtVal <- lookup wrtVar vars = zeros (rows val) (rows wrtVal)
    | otherwise = error $ "Variable " ++ show var ++ " or " ++ show wrtVar ++ " not set."
-- Chain rule: ddx f(e) = dde f * ddx e
jacobianWrtVar (EFun f e) vars var = dde_f <> ddx_e
  where
    dde_f = getJacobian f val
    ddx_e = jacobianWrtVar e vars var
    val   = evaluate e vars
-- Special case for addition.
jacobianWrtVar (EAdd l r) vars var = ddx_l + ddx_r
  where
    ddx_l = jacobianWrtVar l vars var
    ddx_r = jacobianWrtVar r vars var
