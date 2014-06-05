module HVX.DcpTests.OkAffineAffine where

import Numeric.LinearAlgebra

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = (EConst $ (1><2) [1,2]) *~ x
  return ()
