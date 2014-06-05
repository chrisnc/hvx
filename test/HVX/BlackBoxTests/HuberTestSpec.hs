{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HVX.BlackBoxTests.HuberTestSpec where

import Data.Maybe
import Test.Hspec
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util (zeros)

import HVX
import HVX.Internal.Matrix
import HVX.Internal.Util

-- Problem definition.
n = 4
a = EConst $ (n><n)
    ([0.5377  , 0.3188  , 3.5784  , 0.7254
    , 1.8339  , -1.3077 , 2.7694  , -0.0631
    , -2.2588 , -0.4336 , -1.3499 , 0.7147
    , 0.8622  , 0.3426  , 3.0349  , -0.2050 ] :: [Double])
b = EConst $
    (n><1) ([ -1.0689 , -0.8095 , -2.9443 , -1.4384 ] :: [Double])
c = EConst $ (n><n)
    ([ -0.1241 , 0.6715  , 0.4889  , 0.2939
    , 1.4897  , -1.2075 , 1.0347  , -0.7873
    , 1.4090  , 0.7172  , 0.7269  , 0.8884
    , 1.4172  , 1.6302  , -0.3034 , -1.1471 ] :: [Double])
d = EConst $ (n><1)
    ([ 0.3252  , -0.7549 , 1.3703  , -1.7115 ] :: [Double])

x = EVar "x"
y = EVar "y"
constZeroVector = EConst $ zeros n 1

subgradAns = subgradMinimize
  (hmax $ huber 3 (a *~ x +~ b) +~ berhu 1 (c *~ y +~ d))
  [x +~ y <=~ constZeroVector]
  (decNonSumStep 10.0) 5000
  [("x", zeros n 1), ("y", zeros n 1)]

ellipsoidAns = ellipsoidMinimize
  (hmax $ huber 3 (a *~ x +~ b) +~ berhu 1 (c *~ y +~ d))
  [x +~ y <=~ constZeroVector]
  [("x", n), ("y", n)]
  1e-16 1e10

(subgradVars, subgradOptval) = subgradAns
subgradOptx = fromMaybe (error "wat") $ lookup "x" subgradVars
subgradOpty = fromMaybe (error "wat") $ lookup "y" subgradVars

(ellipsoidVars, ellipsoidOptval, ellipsoidUBound) = ellipsoidAns
ellipsoidOptx = fromMaybe (error "wat") $ lookup "x" ellipsoidVars
ellipsoidOpty = fromMaybe (error "wat") $ lookup "y" ellipsoidVars

-- CVX's results.
cvxOptval = 0.404882
cvxOptx = (n><1) [-1.6137 ,-1.1202 ,0.7986 ,-0.7487 ]
cvxOpty = (n><1) [0.2428 ,-0.0191 ,-0.7986 ,-1.0079 ]

-- Verify that HVX matches CVX.
spec :: Spec
spec = do
  describe "Verify that HVX subgrad matches CVX for huber/berhu" $ do
    it "HVX's x should match CVX's x" $
      subgradOptx `shouldSatisfy` fpequalsMat cvxOptx
    it "HVX's y should match CVX's y" $
      subgradOpty `shouldSatisfy` fpequalsMat cvxOpty
    it "HVX's optval should match CVX's optval" $
      subgradOptval `shouldSatisfy` fpequals cvxOptval

  describe "Verify that HVX ellipsoid matches CVX for huber/berhu" $ do
    it "HVX's x should match CVX's x" $
      ellipsoidOptx `shouldSatisfy` fpequalsMat cvxOptx
    it "HVX's y should match CVX's y" $
      ellipsoidOpty `shouldSatisfy` fpequalsMat cvxOpty
    it "HVX's optval should match CVX's optval" $
      ellipsoidOptval `shouldSatisfy` fpequals cvxOptval

main :: IO ()
main = hspec spec
