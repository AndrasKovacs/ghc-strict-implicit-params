# ghc-strict-implicit-params
GHC plugin for making implicit parameters strict.

Usage: add this package as a dependency to `cabal` or `stack`, then enable the `-fplugin StrictImplParams` GHC option.

This turns all implicit parameters on top-level definitions strict.

**Note**: it only works on top-level definitions. It doesn't work on local let-s
and lambda expressions. I'll try to add support for these in the future.
