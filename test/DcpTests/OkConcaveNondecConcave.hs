module HVX.DcpTests.OkConcaveNondecConcave where

import HVX

main :: IO ()
main = do
  let x = EVar "x"
      _ = hlog $ hlog x
      _ = hlog $ hmin x
  return ()
