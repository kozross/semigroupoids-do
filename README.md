# `semigroupoids-do` 

## What is this thing?

With GHC 9, we finally got the
[QualifiedDo](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html)
extension, allowing us to overload do-notation in a convenient way. This has a
range of uses; in our case, we are interested in being able to use do-notation
in a more general context than would normally be possible.

[`semigroupoids`](https://hackage.haskell.org/package/semigroupoids) as a
package is designed to generalize certain common type classes, such as
`Applicative` and `Monad`. This can be convenient: for example, something could 
be an `Apply` or `Bind`, but not an `Applicative` or `Monad` if no sensible 
definition for `pure` exists. However, without the convenience of do-notation, 
such instances mean significantly less, as they are awkward to use. This is an 
unnecessary restriction at least in this example, as nothing about do-notation
requires `pure`.

In this package, we provide a set of definitions suitable for use with
`QualifiedDo`, generalizing all the necessary operations to their
`semigroupoids` equivalents. Specifically:

* It is now possible to use do-notation when you only have a
  [`Bind`](https://hackage.haskell.org/package/semigroupoids-5.3.5/docs/Data-Functor-Bind.html#t:Bind).
* With `ApplicativeDo`, in some cases, you can use do-notation when you only
  have an
  [`Apply`](https://hackage.haskell.org/package/semigroupoids-5.3.5/docs/Data-Functor-Bind.html#t:Apply),
  subject to the same conditions as using `Applicative` for do-notation.
* You can use refutable patterns when you only have
  [`Plus`](https://hackage.haskell.org/package/semigroupoids-5.3.5/docs/Data-Functor-Plus.html#t:Plus).

## What are the goals of this project?

Our only goal is to provide `QualifiedDo` support for `semigroupoid`
generalizations; no more, no less.

## How do I use this?

```haskell
{-# LANGUAGE QualifiedDo #-}

module MyModule where

import Data.Functor.Bind (Bind)
import qualified Semigroupoids.Do as S

foo :: (Bind m) => m a
foo = S.do
  ...
```

## Wait, haven't I seen this somewhere before?

You are probably thinking of
[`semigroupoids-syntax`](https://hackage.haskell.org/package/semigroupoids-syntax).
It is true that we take inspiration from it. However, we differ from it in a
couple of ways:

* We only support the subset of syntax modification allowed by `QualifiedDo`;
  `semigroupoids-syntax` goes beyond this, supplying a range of
  `RebindableSyntax` capabilities.
* `semigroupoids-syntax` hasn't been updated since 2014.
* `semigroupoids-syntax` has support going back to much older GHCs; we limit
  ourselves only to GHCs that have `QualifiedDo`.

In short, this is a maintained, small, and simple package, to do only one task.

## What does this run on?

We support (and CI check) only those versions of GHC which support
`QualifiedDo`. Currently, that list is:

* 9.0.1

We check on the following platforms:

* Windows
* Linux
* MacOS

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.
