# ghc-strict-implicit-params
GHC plugin for making implicit parameters strict.

Usage: add this package as a dependency to `cabal` or `stack`, then enable the `-fplugin
StrictImplParams` GHC option.

This plugin strictifies implicit parameters on top-level definitions and local "let" or "where"
definitions.

**Please note**:

- GHC versions starting from 9.4.x are supported.
- This plugin only works on immediately bound definitions. It does not work on lambda expressions
  passed as function arguments, like `f (\x -> ...)` where `f` has type `((?foo :: Int) => a) ->
  ...`. In those cases, you need to let-bind the lambda expression or insert `seq ?foo` inside the
  body manually.
- This plugin does not work on *nested* constraints containing implicit parameters.
  For example `f :: (?foo :: Int, ?bar :: Int, ?baz :: Int) => ...` is fine, but
  in `f :: (?foo :: Int, (?bar :: Int, ?baz :: Int)) => ...` only `?foo` gets forced.
- Only curried implicit parameters are supported. Example: `(?x :: Int, ?y :: Int) => Int` is not
  strictified, but `(?x :: Int) => (?y :: Int) => Int` is. The current implementation looks for
  arguments with an implicit parameter type, but a pair of dictionaries is not of this form anymore.
- Implicit let-bindings are not strictified! This is usually not an issue if all functions which
  take implicit parameters are strict, because that causes non-dead let bindings to be
  forced. However, you should be still mindful of passing implicit parameters to statically unknown
  functions; those are still lazy.

It's possible that some of the above limitations will be lifted in future versions.
