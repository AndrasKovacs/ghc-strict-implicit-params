# ghc-strict-implicit-params
GHC plugin for making implicit parameters strict.

Usage: add this package as a dependency to `cabal` or `stack`, then enable the `-fplugin StrictImplParams` GHC option.

This turns all implicit parameters on top-level definitions strict.

**Note**: WIP. Doesn't yet work for all input.


