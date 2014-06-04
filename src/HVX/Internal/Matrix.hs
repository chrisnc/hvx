module HVX.Internal.Matrix
  ( Mat
  , allMat
  , anyMat
  , diagMat
  , ei
  , fpequalsMat
  , lpnorm
  , matrixPow
  , reduceMat
  , scalarMat
  , zeroMat
  , zeroVec
  ) where

import Numeric.LinearAlgebra hiding (i)
import Numeric.LinearAlgebra.Util

import HVX.Internal.Util

type Mat = Matrix Double

allMat :: (Double -> Bool) -> Mat -> Bool
allMat f x = all f (toList . flatten $ x)

anyMat :: (Double -> Bool) -> Mat -> Bool
anyMat f x = any f (toList . flatten $ x)

diagMat :: Mat -> Mat
diagMat = diag . flatten

ei :: Int -> Int -> Mat
ei n i = buildMatrix n 1 (\(j, _) -> if i == j then 1 else 0)

fpequalsMat :: Mat -> Mat -> Bool
fpequalsMat a b
  | ra == rb && ca == cb = all (uncurry fpequals) $ zip alist blist
  | otherwise = error "Two matrices with different dimensions cannot possibley be equal!"
  where
    ra = rows a
    rb = rows b
    ca = cols a
    cb = cols b
    alist = toList . flatten $ a
    blist = toList . flatten $ b

lpnorm :: Double -> Mat -> Double
lpnorm p x = sumElements y ** (1/p)
  where pMat = (1><1) [p]
        y = abs x ** pMat

matrixPow :: Double -> Mat -> Mat
matrixPow p = mapMatrix (** p)

reduceMat :: ([Double] -> Double) -> Mat -> Mat
reduceMat f = (1><1) . (:[]) . f . toList . flatten

scalarMat :: Double -> Mat
scalarMat x = (1><1) [x]

zeroMat :: Int -> Mat
zeroMat n = zeros n n

zeroVec :: Int -> Mat
zeroVec n = zeros n 1
