HVX
===
Disciplined Convex Programming and Symbolic Subdifferentiation in Haskell
-------------------------------------------------------------------------

This is the preferred version of HVX. An older version (compatible with GHC \>=7.6.3) is available at https://github.com/chrisnc/hvx-ghc-763, as well as a version compatible with GHC \>7.8.2 in the git history of https://github.com/chrisnc/hvx.

This version of HVX requires GHC \>=8.2.1 because it uses a recent version of hmatrix.

To install HVX:
 - Install GHC 8.2.1 and cabal
 - Install your operating system's LAPACK and GSL packages.
 - cabal sandbox init
 - cabal configure --enable-tests
 - cabal install

See the examples directory for ways to use this package.

Run the demos with:

    cabal run demo

or:

    cabal run subgrad
