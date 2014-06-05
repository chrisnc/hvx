module HVX.DcpTests.NoConvexNondecConcave where

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = hexp $ hlog x
  return ()
