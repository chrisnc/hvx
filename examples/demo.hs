import Numeric.LinearAlgebra
import HVX

n = 4
a = EConst $ (n><n) ([0, 2, 0, 0,
                      -3, 0, 0, 0,
                       0, 0, 1, 0,
                       0, 0, 0, 4] :: [Double])
b = EConst $ (n><n) ([4, 8, -1, 0,
                      0, -2, 1, 0,
                      9, -3, 0, 3,
                      0, 0,  2, 0] :: [Double])
c = EConst $ (n><1) [1..]

x = EVar "x"
y = EVar "y"
constNeg3 = EConst $ scale (-3.0) $ konst 1.0 (n, 1)
constTwos = EConst $ scale 2.0 $ konst 1.0 (n, 1)

ans = subgradMinimize
  (norm 2 (a *~ x) +~ norm 2 (b *~ y) +~ norm 2 (c +~ x +~ neg y))
  [y <=~ constTwos, x <=~ constNeg3]
  (decNonSumStep 10.0) 5000
  [("x", konst 0.0 (n, 1)), ("y", konst 0.0 (n, 1))]

main :: IO ()
main = print ans
