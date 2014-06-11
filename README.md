Uncertain
=========

Provides tools to manipulate numbers tagged with inherent
experimental/measurement uncertainty, and propagates them through functions
based on principles from statistics.

### Create numbers

```haskell
7.13 +/- 0.05
91800 +/- 100
certain 7.9512
81.42 `withPrecision` 4
7    :: Uncertain Double
9.18 :: Uncertain Double
```

### Apply functions and have the errors propagate properly

```haskell
> let x = 1.52 +/- 0.07
> let y = 781.4 +/- 0.3
> let z = 1.53e-1 `withPrecision` 3
> cosh x
2.4 +/- 0.2
> exp x / z * sin (y ** z)
10.8 +/- 0.9
> pi + 3 * logBase x y
50.87 +/- 2.e-2
```

### Apply arbitrary numeric functions

```haskell
> let f :: Double -> Double -> Double -> Double;
      f a b c = b ** exp (c / (2 * a))
> y ** (exp z / (x * 2))
13.0 +/- 2.
> uMap3 f x y z
13.0 +/- 2.
```

## Comparisons

Note that this is very different from other libraries with similar data types
(like `interval` and `rounded`); these do not attempt to maintain intervals or
simply digit precisions; they instead are intended to model actual
experimental and measurement data with their uncertainties, and apply
functions to the data with the uncertainties and properly propagating the
errors with sound statistical principles.

As a clear example, take

```haskell
> (52 +/- 6) + (39 +/- 4)
91. +/- 7.
```

In a library like `interval`, this would result in `91 +/- 10` (that is, a
lower bound of 46 + 35 and an upper bound of 58 + 43).  However, with
experimental data, two points of uncertainty 6 and 4 will add to create a new
point of uncertainty 7.

