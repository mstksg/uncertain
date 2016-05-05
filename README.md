Uncertain
=========

[![Build Status](https://travis-ci.org/mstksg/uncertain.svg?branch=master)](https://travis-ci.org/mstksg/uncertain)

Provides tools to manipulate numbers with inherent experimental/measurement
uncertainty, and propagates them through functions based on principles from
statistics.

Documentation maintained at <https://mstksg.github.io/uncertain>.

## Usage

```haskell
import Data.Uncertain
```

### Create numbers

```haskell
7.13 +/- 0.05
91800 +/- 100
12.5 `withVar` 0.36
'exact' 7.9512
81.42 `withPrecision` 4
7    :: Uncertain Double
9.18 :: Uncertain Double
fromSamples [12.5, 12.7, 12.6, 12.6, 12.5]
```

Can be descontructed/analyzed with `:+/-` (pattern synonym/pseudo-constructor
matching on the mean and standard deviation), `uMean`, `uStd`, `uVar`, etc.

### Manipulate with error propagation

```haskell
ghci> let x = 1.52 +/- 0.07
ghci> let y = 781.4 +/- 0.3
ghci> let z = 1.53e-1 `withPrecision` 3
ghci> cosh x
2.4 +/- 0.2
ghci> exp x / z * sin (y ** z)
10.9 +/- 0.9
ghci> pi + 3 * logBase x y
52 +/- 5
```

Propagates uncertainty using second-order multivariate Taylor expansions of
functions, computed using the *[ad][]* library.

[ad]: https://hackage.haskell.org/package/ad

#### Arbitrary numeric functions

```haskell
ghci> liftUF (\[x,y,z] -> x*y+z)
             [ 12.2 +/- 0.5
             , 56 +/- 2
             , 0.12 +/- 0.08
             ]
680 +/- 40
```

## Correlated samples

Can propagate uncertainty on complex functions take from potentially correlated
samples.

```haskell
ghci> import Data.Uncertain.Correlated
ghci> evalCorr $ do
        x <- sampleUncert $ 12.5 +/- 0.8
        y <- sampleUncert $ 15.9 +/- 0.5
        z <- sampleUncert $ 1.52 +/- 0.07
        let k = y ** x
        resolveUncert $ (x+z) * logBase z k
1200 +/- 200
```

### "Interactive" Exploratory Mode

*Correlated* module functionality can be used in *ghci* or `IO` or `ST`, for
"interactive" exploration.

```haskell
ghci> x <- sampleUncert $ 12.5 +/- 0.8
ghci> y <- sampleUncert $ 15.9 +/- 0.5
ghci> z <- sampleUncert $ 1.52 +/- 0.07
ghci> let k = y**x
ghci> resolveUncert $ (x+z) * logBase z k
1200 +/- 200
```

## Monte Carlo-based propagation of uncertainty

Provides a module for propagating uncertainty using [Monte Carlo
simulations][]

[Monte Carlo simulations]: https://en.wikipedia.org/wiki/Monte_Carlo_method

```haskell
ghci> import qualified Data.Uncertain.MonteCarlo as MC
ghci> import System.Random.MWC
ghci> let x = 1.52 +/- 0.07
ghci> let y = 781.4 +/- 0.3
ghci> let z = 1.53e-1 `withPrecision` 3
ghci> g <- create
ghci> cosh x
2.4 +/- 0.2
ghci> MC.liftU cosh x g
2.4 +/- 0.2
ghci> exp x / z * sin (y ** z)
10.9 +/- 0.9
ghci> MC.liftU3 (\a b c -> exp a / c * sin (b**c)) x y z g
10.8 +/- 1.0
ghci> pi + 3 * logBase x y
52 +/- 5
ghci> MC.liftU2 (\a b -> pi + 3 * logBase a b) x y g
51 +/- 5
```

## Comparisons

Note that this is very different from other libraries with similar data types
(like from [intervals][] and [rounding][]); these do not attempt to maintain intervals or
simply digit precisions; they instead are intended to model actual
experimental and measurement data with their uncertainties, and apply
functions to the data with the uncertainties and properly propagating the
errors with sound statistical principles.

[intervals]: https://hackage.haskell.org/package/intervals
[rounding]: https://hackage.haskell.org/package/rounding

For a clear example, take

```haskell
> (52 +/- 6) + (39 +/- 4)
91. +/- 7.
```

In a library like [interval], this would result in `91 +/- 10` (that is, a
lower bound of 46 + 35 and an upper bound of 58 + 43).  However, with
experimental data, errors in two independent samples tend to "cancel out", and
result in an overall aggregate uncertainty in the sum of approximately 7.

## Copyright

Copyright (c) Justin Le 2016
