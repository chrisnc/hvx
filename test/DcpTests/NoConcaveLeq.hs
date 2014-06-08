module HVX.DcpTests.OkXAffine where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

import HVX

main :: IO ()
main = do
  let zero = EConst $ zeros 2 1
      x = EVar "x"
      e = hexp $ x
      _ = zero <=~ e
  return ()
