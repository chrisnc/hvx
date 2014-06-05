module HVX.Internal.TestUtil
  ( fpequalsApprox
  , pairsOf
  , positiveVectors
  , smallEvenPowersGreaterThanOne
  , smallNonIntPowersGreaterThanOne
  , smallVectors
  , vectors
  ) where

import Control.Monad
import Numeric.LinearAlgebra
import Test.QuickCheck hiding ( (><) )

import HVX.Internal.Matrix
import HVX.Internal.Util

fpequalsApprox :: Double -> Double -> Bool
fpequalsApprox a b
  | a == 0 && b == 0           = True
  | max (abs a) (abs b) < 1e-6 = True
  | otherwise                  = abs ( (a - b) / denom) < tolerance
  where
    denom = max (abs a) (abs b)
    tolerance = 5e-2  -- 5% tolerance is why we call it Approx.

pairsOf :: Gen a -> Gen b -> Gen (a, b)
pairsOf a b = do
  x <- a
  y <- b
  return (x, y)

positiveVectors :: Gen Mat
positiveVectors = do
  nonEmptyL <- arbitrary :: Gen (NonEmptyList Double)
  let l = getNonEmpty nonEmptyL
  return $ abs $ (length l><1) l

smallEvenPowersGreaterThanOne :: Gen Double
smallEvenPowersGreaterThanOne =
  liftM fromIntegral $
    (arbitrary :: Gen Integer) `suchThat` \x -> x > 1 && x <= 20 && even x

smallNonIntPowersGreaterThanOne :: Gen Double
smallNonIntPowersGreaterThanOne =
  arbitrary `suchThat` \x -> x > 1 && x <= 20 && not (isInteger x)

smallVectors :: Gen Mat
smallVectors = do
  len <- choose (1, 100)
  l <- sequence [choose (-10, 10) | _ <- [1..len]]
  return $ (len><1) l

vectors :: Gen Mat
vectors = do
  nonEmptyL <- arbitrary :: Gen (NonEmptyList Double)
  let l = getNonEmpty nonEmptyL
  return $ (length l><1) l
