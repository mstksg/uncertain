{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Numeric.Uncertain.MonteCarlo
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides an interface for computing and propagating uncertainty by using
-- <https://en.wikipedia.org/wiki/Monte_Carlo_method Monte Carlo simulations>.
-- These may be more accurate than the normal propagators if the third-order
-- taylor series expansion terms are non-negligible.
--
-- Basically simulates sampling from the distribution represented by the given
-- 'Uncert's, applying the function of interest, and aggregating the mean
-- and standard deviation of the results.  @x '+/-' dx@ is treated as
-- a random variable whose probability density is the normal distribution
-- with mean @x@ and standard deviation @dx@.
--
-- This module attempts to duplicate the API offered by "Numeric.Uncertain"
-- and is meant to be imported qualified alongside "Numeric.Uncertain"
--
-- @
-- import           Numeric.Uncertain
-- import qualified Numeric.Uncertain.MonteCarlo as MC
-- @
--
-- Actions are parameterized over all 'PrimMonad' instances, so can be run
-- under both 'ST' and 'IO', making it suitable for exploratory purposes.
-- All functions require a 'Gen' from "System.Random.MWC" for random value
-- generation purposes.
--
-- @
-- ghci> import qualified Numeric.Uncertain.MonteCarlo as MC
-- ghci> import System.Random.MWC
-- ghci> let x = 1.52 '+/-' 0.07
-- ghci> let y = 781.4 +/- 0.3
-- ghci> let z = 1.53e-1 `'withPrecision'` 3
-- ghci> g <- 'create'
-- ghci> cosh x
-- 2.4 +/- 0.2
-- ghci> MC.liftU cosh x g
-- 2.4 +/- 0.2
-- ghci> exp x / z * sin (y ** z)
-- 10.9 +/- 0.9
-- ghci> MC.liftU3 (\\a b c -> exp a / c * sin (b**c)) x y z g
-- 10.8 +/- 1.0
-- ghci> pi + 3 * logBase x y
-- 52 +/- 5
-- ghci> MC.liftU2 (\\a b -> pi + 3 * logBase a b) x y g
-- 51 +/- 5
-- @
module Numeric.Uncertain.MonteCarlo (
  -- * Sampling from an 'Uncert'
  sampleUncert,

  -- * Lifting functions via Monte Carlo simulation

  -- ** Fixed iterations
  liftU,
  liftU2,
  liftU3,
  liftU4,
  liftU5,
  liftUF,

  -- ** Variable iterations
  liftU',
  liftU2',
  liftU3',
  liftU4',
  liftU5',
  liftUF',
)
where

import Control.Monad
import Control.Monad.Primitive
import Data.Hople
import Numeric.Uncertain (Uncert, fromSamples, uMeanStd)
import Prelude.Compat
import System.Random.MWC
import System.Random.MWC.Distributions

-- | Sample a random 'Double' from the distribution specified by an
-- @'Uncert' 'Double'@.  @x '+/-' dx@ is treated as a random variable whose
-- probability density is the normal distribution with mean @x@ and
-- standard deviation @dx@.
sampleUncert ::
  PrimMonad m =>
  Uncert Double ->
  Gen (PrimState m) ->
  m Double
sampleUncert (uMeanStd -> (x, dx)) = normal x dx
{-# INLINEABLE sampleUncert #-}

-- | Lifts a numeric function over an 'Uncert' using a Monte Carlo
-- simulation with 1000 samples.
--
-- @
-- ghci> g <- 'create'
-- ghci> MC.liftU (\\x -> log x ^ 2) (12.2 +/- 0.5) g
-- 6.3 +/- 0.2
-- @
liftU ::
  PrimMonad m =>
  (Double -> Double) ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU = liftU' 1000
{-# INLINE liftU #-}

-- | Lifts a multivariate numeric function on a container (given as an @f
-- a -> a@) to work on a container of 'Uncert's using a Monte Carlo
-- simulation with 1000 samples.
--
-- @
-- ghci> g <- 'create'
-- ghci> M.liftUF (\\[x,y,z] -> x*y+z) [12.2 +/- 0.5, 56 +/- 2, 0.12 +/- 0.08] g
-- 680 +/- 40
-- @
liftUF ::
  (Traversable f, PrimMonad m) =>
  (f Double -> Double) ->
  f (Uncert Double) ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftUF = liftUF' 1000
{-# INLINE liftUF #-}

-- | Lifts a two-argument (curried) function over two 'Uncert's using
-- a Monte Carlo simulation with 1000 samples.
--
-- @
-- ghci> g <- 'create'
-- ghci> MC.liftU2 (\\x y -> x**y) (13.5 +/- 0.1) (1.64 +/- 0.08)
-- 70 +/- 20
-- @
liftU2 ::
  PrimMonad m =>
  (Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU2 = liftU2' 1000
{-# INLINE liftU2 #-}

-- | Lifts a three-argument (curried) function over three 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU3 ::
  PrimMonad m =>
  (Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU3 = liftU3' 1000
{-# INLINE liftU3 #-}

-- | Lifts a four-argument (curried) function over four 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU4 ::
  (PrimMonad m, Applicative m) =>
  (Double -> Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU4 = liftU4' 1000
{-# INLINE liftU4 #-}

-- | Lifts a five-argument (curried) function over five 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU5 ::
  PrimMonad m =>
  (Double -> Double -> Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU5 = liftU5' 1000
{-# INLINE liftU5 #-}

-- | Like 'liftU', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftU' ::
  PrimMonad m =>
  Int ->
  (Double -> Double) ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU' n f u g = fromSamples <$> replicateM n samp
  where
    samp = f <$> sampleUncert u g
{-# INLINEABLE liftU' #-}

-- | Like 'liftUF', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftUF' ::
  (Traversable f, PrimMonad m) =>
  Int ->
  (f Double -> Double) ->
  f (Uncert Double) ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftUF' n f us g = fromSamples <$> replicateM n samp
  where
    samp = f <$> traverse (`sampleUncert` g) us
{-# INLINEABLE liftUF' #-}

-- | Like 'liftU2', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftU2' ::
  PrimMonad m =>
  Int ->
  (Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU2' n f x y = liftUF' n (uncurryH2 f) (H2 x y)
{-# INLINEABLE liftU2' #-}

-- | Like 'liftU3', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftU3' ::
  PrimMonad m =>
  Int ->
  (Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU3' n f x y z = liftUF' n (uncurryH3 f) (H3 x y z)
{-# INLINEABLE liftU3' #-}

-- | Like 'liftU4', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftU4' ::
  PrimMonad m =>
  Int ->
  (Double -> Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU4' n f x y z a = liftUF' n (uncurryH4 f) (H4 x y z a)
{-# INLINEABLE liftU4' #-}

-- | Like 'liftU5', but allows you to specify the number of samples to run
-- the Monte Carlo simulation with.
liftU5' ::
  PrimMonad m =>
  Int ->
  (Double -> Double -> Double -> Double -> Double -> Double) ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Uncert Double ->
  Gen (PrimState m) ->
  m (Uncert Double)
liftU5' n f x y z a b = liftUF' n (uncurryH5 f) (H5 x y z a b)
{-# INLINEABLE liftU5' #-}
