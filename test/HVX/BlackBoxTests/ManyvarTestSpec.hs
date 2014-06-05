{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HVX.BlackBoxTests.ManyvarTestSpec where

import Test.Hspec
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util (ones, zeros)

import HVX
import HVX.Internal.TestUtil

-- Problem definition.
nx = 2
ny = 3
nz = 4
a = EConst $ (3><nx)
    ([-0.1022 , 0.3129
    , -0.2414 , -0.8649
    , 0.3192 , -0.0301 ] :: [Double])
b = EConst $ (3><ny)
    ([-0.1649 , 1.1093 , -1.2141
    , 0.6277 , -0.8637 , -1.1135
    , 1.0933 , 0.0774 , -0.0068 ] :: [Double])
c = EConst $ (3><nz)
    ([1.5326 , -0.2256 , 0.0326 , 1.5442
    , -0.7697 , 1.1174 , 0.5525 , 0.0859
    , 0.3714 , -1.0891 , 1.1006 , -1.4916 ] :: [Double])
d = EConst $ (3><1)
    ([-0.6156 , 0.7481 , -0.1924] :: [Double])

x = EVar "x"
y = EVar "y"
z = EVar "z"
const10nx = EConst $ scale 10 $ ones nx 1
const10ny = EConst $ scale 10 $ ones ny 1
const01nz = EConst $ scale 0.1 $ ones nz 1

subgradAns = subgradMaximize
  ( neg (norm 2 (a *~ x +~ b *~ y +~ c *~ z +~ d))
    +~ neg (norm 1 x)
    +~ neg (norm 4.2 y) )
  [ const01nz >=~ hexp z
    , powBaseP01 0.25 x >=~ const10nx
    , powBaseP1InfNotInt 1.75 y <=~ const10ny ]
  (decNonSumStep 100.0) 10
  [("x", zeros nx 1), ("y", zeros ny 1), ("z", zeros nz 1)]

ellipsoidAns = ellipsoidMaximize
  ( neg (norm 2 (a *~ x +~ b *~ y +~ c *~ z +~ d))
    +~ neg (norm 1 x)
    +~ neg (norm 4.2 y) )
  [ const01nz >=~ hexp z
    , powBaseP01 0.25 x >=~ const10nx
    , powBaseP1InfNotInt 1.75 y <=~ const10ny ]
  [("x", nx), ("y", ny), ("z", nz)]
  1e-16 1e10

(subgradVars, subgradOptval) = subgradAns

(ellipsoidVars, ellipsoidOptval, ellipsoidUBound) = ellipsoidAns

-- CVX's results.
cvxOptval = -29051

-- Verify that HVX matches CVX.
-- TODO(mh): This test fails because primitives that generate implicit
-- constraints are currently unsupported. When support is added for them, the
-- test will be added back in. (2014-06-04)
spec :: Spec
spec =
  describe "Placeholder test" $ do
    it "placeholder description" $
      True `shouldBe` True
--  describe "Verify that HVX matches CVX for triple variable stress test" $ do
--    it "Verify that HVX subgrad matches CVX for huber/berhu" $
--      subgradOptval `shouldSatisfy` fpequalsApprox cvxOptval
--    it "Verify that HVX ellipsoid matches CVX for huber/berhu" $
--      ellipsoidOptval `shouldSatisfy` fpequalsApprox cvxOptval
--
--main :: IO ()
--main = hspec spec
