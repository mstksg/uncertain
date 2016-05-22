Version 0.3.1.0
===============

<https://github.com/mstksg/uncertain/releases/tag/v0.3.1.0>

*   Added support for *GHC 8.0* by providing pattern synonym type signatures in
    the proper format.
*   `(:+/-)` pattern synonym now exported as a "constructor" with `Uncert`
*   Generalized the type signatures for `liftCX` functions to work for all `a`.
    Restriction to `Fractional` now occurs only at exit points of the `CVar`
    abstraction.
*   Removed the redundant constraint on `Functor m` for the *MonteCarlo*
    module's `liftUX` functions.

Version 0.3.0.0
===============

<https://github.com/mstksg/uncertain/releases/tag/v0.3.0.0>

*   **(Breaking change)** Moved the top-level modules from *Data* to *Numeric*,
    to better reflect the nature of the library and to align with the
    convention of other similar libraries.

Version 0.2.0.0
===============

<https://github.com/mstksg/uncertain/releases/tag/v0.2.0.0>

*   Initial release, re-written from the unreleased `0.1.0.0` by
    re-implementing error propagation with the [ad][] library.

[ad]: https://hackage.haskell.org/package/ad

