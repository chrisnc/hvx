HVX
===
Disciplined Convex Programming and Symbolic Subdifferentiation in Haskell
-------------------------------------------------------------------------

This is the preferred (newest) version of HVX. An older version (compatible with
GHC >=7.6.3) is available at https://github.com/chrisnc/hvx-ghc-763.

This version requires GHC >=7.8.2 because it uses closed type families. Until
GHC 7.8.2 becomes more widely adopted by the various package managers this means
that users will probably have to install GHC 7.8.2 themselves.

To install HVX:
 - Install GHC 7.8.2: https://www.haskell.org/ghc/docs/6.10.1/html/users_guide/installing-bin-distrib.html
 - Install your operating system's LAPACK and GSL packages
 - ./install.sh (no sudo needed)

See examples/example.hs for an example of how to use this code.

Run the example with:
ghci examples/example.hs
