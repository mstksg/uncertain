-- |
-- Module      : Numeric.Uncertain.Correlated.Interactive
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Exports all of the interface of "Numeric.Uncertain.Correlated", except
-- meant to be run in a /ghci/ session "interactively" for exploratory
-- purposes, or in a plain 'IO' action (instead of inside a 'Corr' monad).
--
-- For example, with the "Numeric.Uncertain.Correlated" interface:
--
-- @
-- ghci> evalCorr $ do
--         x <- sampleUncert $ 12.5 +/- 0.8
--         y <- sampleUncert $ 15.9 +/- 0.5
--         z <- sampleUncert $ 1.52 +/- 0.07
--         let k = y**x
--         resolveUncert $ (x+z) * logBase z k
-- 1200 +/- 200
-- @
--
-- And with the interface from this "interactive" module:
--
-- @
-- ghci> x <- 'sampleUncert' $ 12.5 +/- 0.8
-- ghci> y <- sampleUncert $ 15.9 +/- 0.5
-- ghci> z <- sampleUncert $ 1.52 +/- 0.07
-- ghci> let k = y**x
-- ghci> 'resolveUncert' $ (x+z) * logBase z k
-- 1200 +/- 200
-- @
--
-- The main purpose of this module is to allow one to use /ghci/ as a fancy
-- "calculator" for computing and exploring propagated uncertainties of
-- complex and potentially correlated samples with uncertainty.
--
-- Because many of the names overlap with the names from the
-- "Numeric.Uncertain.Correlated" module, it is recommended that you never
-- have both imported at the same time in /ghci/ or in a file, or import
-- them qualified if you must.
--
-- Also note that all of these methods only work with @'Uncertain'
-- 'Double'@s, and are not polymorphic over different numeric types.
--
-- Be aware that this module is not robustly tested in heavily concurrent
-- situations/applications.
--
module Numeric.Uncertain.Correlated.Interactive
  ( -- * Uncertain and Correlated Values
    CVar, CVarIO
    -- ** Sampling
  , sampleUncert, sampleExact, constC
    -- ** Resolving
  , resolveUncert
    -- * Applying arbitrary functions
  , liftC, liftC2, liftC3, liftC4, liftC5, liftCF
  )
  where

import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Tuple
import           Numeric.Uncertain
import           Numeric.Uncertain.Correlated.Internal
import           System.IO.Unsafe                      (unsafePerformIO)
import qualified Data.IntMap.Strict                    as M
import qualified Numeric.Uncertain.Correlated          as C

-- | A 'CVar' specialized to work in an "interactive" context, in /ghci/ or
-- 'IO'.
type CVarIO = CVar RealWorld Double

-- ssh, don't tell anyone we're using 'unsafePerformIO'
globalCorrMap :: IORef (M.Key, M.IntMap (Uncert Double))
{-# NOINLINE globalCorrMap #-}
globalCorrMap = unsafePerformIO $ newIORef (0, M.empty)

runCorrIO :: Corr RealWorld Double a -> IO a
runCorrIO c = atomicModifyIORef' globalCorrMap
                                 (swap . runState (corrToState c))
{-# INLINE runCorrIO #-}

-- | Generate a sample in 'IO' from an @'Uncert' 'Double'@ value,
-- independently from all other samples.
sampleUncert :: Uncert Double -> IO CVarIO
sampleUncert u = runCorrIO $ C.sampleUncert u
{-# INLINABLE sampleUncert #-}

-- | Generate an exact sample in 'IO' with zero uncertainty,
-- independently from all other samples.
--
-- Not super useful, since you can do something equivalent with 'constC'
-- or the numeric instances:
--
-- @
-- sampleExact x  ≡ return ('constC' x)
-- sampleExact 10 ≡ return 10
-- @
--
-- But is provided for completeness alongside 'sampleUncert'.
sampleExact :: Double -> IO CVarIO
sampleExact d = runCorrIO $ C.sampleExact d
{-# INLINABLE sampleExact #-}

-- | "Resolve" an 'Uncert' from a 'CVarIO' using its potential multiple
-- samples and sample sources, taking into account inter-correlations
-- between 'CVarIO's and samples.
--
-- Note that if you use 'sampleUncert' on the result, the new sample will
-- be treated as something completely independent.  Usually this should
-- only be used as the "final value" of your computation or exploration.
resolveUncert :: CVarIO -> IO (Uncert Double)
resolveUncert v = runCorrIO $ C.resolveUncert v
{-# INLINABLE resolveUncert #-}
