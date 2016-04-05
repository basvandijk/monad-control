[![Hackage](https://img.shields.io/hackage/v/monad-control.svg)](https://hackage.haskell.org/package/monad-control)
[![Build Status](https://travis-ci.org/basvandijk/monad-control.svg)](https://travis-ci.org/basvandijk/monad-control)

This package defines the type class `MonadControlIO`, a subset of
`MonadIO` into which generic control operations such as `catch` can be
lifted from `IO`.  Instances are based on monad transformers in
`MonadTransControl`, which includes all standard monad transformers in
the `transformers` library except `ContT`.

Note that this package is a rewrite of Anders Kaseorg's `monad-peel`
library.  The main difference is that this package provides CPS style
operators and exploits the `RankNTypes` language extension to simplify
most definitions.

The package includes a copy of the `monad-peel` testsuite written by
Anders Kaseorg The tests can be performed by using `cabal test`.

[This `criterion`](https://github.com/basvandijk/bench-monad-peel-control)
based benchmark shows that `monad-control` is on average about 2.5
times faster than `monad-peel`.
