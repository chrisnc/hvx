module HVX.Internal.TestUtil
  ( pairsOf
  , smallEvenPowersGreaterThanOne
  , smallNonIntPowersGreaterThanOne
  , smallVectors
  , vectors
  , positiveVectors
  ) where

import Control.Monad
import Numeric.LinearAlgebra
import Test.QuickCheck hiding ( (><) )

import HVX.Internal.Matrix
import HVX.Internal.Util

pairsOf :: Gen a -> Gen b -> Gen (a, b)
pairsOf a b = do
  x <- a
  y <- b
  return (x, y)

smallEvenPowersGreaterThanOne :: Gen Double
smallEvenPowersGreaterThanOne =
  liftM fromIntegral $
    (arbitrary :: Gen Integer) `suchThat` \x -> x > 1 && x <= 20 && even x

smallNonIntPowersGreaterThanOne :: Gen Double
smallNonIntPowersGreaterThanOne =
  arbitrary `suchThat` \x -> x > 1 && x <= 20 && not (isInteger x)

vectors :: Gen Mat
vectors = do
  nonEmptyL <- arbitrary :: Gen (NonEmptyList Double)
  let l = getNonEmpty nonEmptyL
  return $ (length l><1) l

positiveVectors :: Gen Mat
positiveVectors = do
  nonEmptyL <- arbitrary :: Gen (NonEmptyList Double)
  let l = getNonEmpty nonEmptyL
  return $ abs $ (length l><1) l

smallVectors :: Gen Mat
smallVectors = do
  len <- choose (1, 100)
  l <- sequence [choose (-10, 10) | _ <- [1..len]]
  return $ (len><1) l
