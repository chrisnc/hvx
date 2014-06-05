module HVX.DcpTests.OkConvexNondecConvex where

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = hexp $ hexp x
      _ = logsumexp $ hexp x
  return ()
