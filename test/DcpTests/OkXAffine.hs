module HVX.DcpTests.OkXAffine where

import Numeric.LinearAlgebra

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = hexp $ x
      _ = logsumexp $ x
      _ = hlog $ x
      _ = hmin $ x
      _ = (EConst $ (1><2) [1,2]) *~ x
  return ()
