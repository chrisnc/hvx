module HVX.DcpTests.ConvexNondecConcaveDisallowed where

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = hexp $ hlog x
      _ = logsumexp $ hlog x
  return ()
