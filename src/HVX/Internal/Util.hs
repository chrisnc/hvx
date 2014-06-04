module HVX.Internal.Util
  ( fpequals
  , infinity
  , isInteger
  ) where

fpequals :: Double -> Double -> Bool
fpequals a b
  | a == 0 && b == 0            = True
  | max (abs a) (abs b) < 1e-14 = True
  | otherwise                   = abs ( (a - b) / denom) < tolerance
  where
    denom = max (abs a) (abs b)
    tolerance = 1e-10

infinity :: Double
infinity = read "Infinity"

isInteger :: Double -> Bool
isInteger x = fpequals x $ fromInteger $ round x
