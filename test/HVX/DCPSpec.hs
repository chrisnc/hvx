module HVX.DCPSpec (main, spec) where

import Test.Hspec

import HVX.Primitives
import HVX.Internal.Primitives
import HVX.Internal.DCP

x :: Expr Affine Nondec
x = EVar "x"
expx :: Expr Convex Nondec
expx = apply Exp x

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "DCP" $ do
  it "can apply convex nondecreasing to convex" $
    getProperties (apply Max expx) `shouldBe` "Convex Nondec"
  it "can apply affine nonincreasing to concave" $
    getProperties (apply Neg $ apply Neg expx) `shouldBe` "Convex Nondec"
