module HVX.DcpTests.NoAffineNonmonConcaveNondec where

import Numeric.LinearAlgebra

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = (EConst $ (1><2) [1,2]) *~ hlog x
  return ()
