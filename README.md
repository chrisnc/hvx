HVX
===
Disciplined Convex Programming and Symbolic Subdifferentiation in Haskell
-------------------------------------------------------------------------

This is the preferred version of HVX. An older version (compatible with GHC \>=7.6.3) is available at https://github.com/chrisnc/hvx-ghc-763.

This version of HVX requires GHC \>=7.8.2 because it uses closed type families.  Until GHC 7.8.2 becomes more widely adopted by the various package managers this means that users will probably have to install GHC 7.8.2 themselves.

To install HVX:
 - Install GHC 7.8.2: http://www.haskell.org/ghc/download_ghc_7_8_2
 - Install the Cabal library and the cabal-install utility: http://www.haskell.org/cabal/download.html
 - Install your operating system's LAPACK and GSL packages.
 - `./install.sh` (no sudo needed).

See the examples directory for ways to use this package.

Run the demo with:

    ghci examples/demo.hs
