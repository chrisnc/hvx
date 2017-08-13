module HVX.DcpTests.NoConcaveLeq where

import Numeric.LinearAlgebra

import HVX

main :: IO ()
main = do
  let zero = EConst $ konst 0.0 (2, 1)
      x = EVar "x"
      e = hexp $ x
      _ = zero <=~ e
  return ()
