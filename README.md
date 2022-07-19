# ghc-strict-implicit-params
GHC plugin for making implicit parameters strict.

Usage: add this package as a dependency to `cabal` or `stack`, then enable the `-fplugin StrictImplParams` GHC option.

This plugin turns implicit parameters on top-level definitions strict.

**Please note**:

- This plugin only works on top-level definitions. It doesn't work on local
  definitions and arbitrary local lambda expressions.
- Only curried implicit parameters are supported. Example: `(?x :: Int, ?y ::
  Int) => Int` is not strictified, but `(?x :: Int) => (?y :: Int) => Int`
  is. The current implementation looks for arguments with an implicit parameter
  type, but a pair of dictionaries is not of this form anymore.
- Implicit let-bindings are not strictified! This is usually not an issue if
  all functions which expect implicit parameters are strict, because that
  causes non-dead let bindings to be forced. However, you should be still
  mindful of passing implicit parameters to unknown functions; those are
  still lazy.

It's possible that some of the above limitations will be lifted in future
versions.
