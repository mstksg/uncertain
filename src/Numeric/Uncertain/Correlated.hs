{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Numeric.Uncertain.Correlated
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the 'Corr' monad, which allows one to describe complex
-- relationships between random variables and evaluate their propagated
-- uncertainties /respecting/ their inter-correlations.
--
-- See the "Numeric.Uncertain.Correlated.Interactive" module for an
-- "interactive" and exploratory interface for this module's functionality.
module Numeric.Uncertain.Correlated (
  -- * 'Corr'
  Corr,
  evalCorr,

  -- * Uncertain and Correlated Values
  CVar,

  -- ** Sampling
  sampleUncert,
  sampleExact,
  constC,

  -- ** Resolving
  resolveUncert,

  -- * Applying arbitrary functions
  liftC,
  liftC2,
  liftC3,
  liftC4,
  liftC5,
  liftCF,
)
where

import Control.Monad.Free
import Control.Monad.Trans.State
import qualified Data.IntMap.Strict as M
import Numeric.Uncertain
import Numeric.Uncertain.Correlated.Internal

-- | Evaluates the value described by a 'Corr' monad, taking into account
-- inter-correlations between samples.
--
-- Takes a universally qualified 'Corr', which should not affect usage.
-- See the examples in the documentation for 'Corr'.  The univeral
-- qualification is mostly a type system trick to ensure that you aren't
-- allowed to ever use 'evalCorr' to evaluate a 'CVar'.
evalCorr :: Fractional a => (forall s. Corr s a b) -> b
evalCorr c = evalState (corrToState c) (0, M.empty)
{-# INLINEABLE evalCorr #-}

-- | Generate a sample in 'Corr' from an 'Uncert' value, independently from
-- all other samples.
--
-- Note that you can only sample @'Uncert' a@s within a @'Corr' s a@, meaning
-- that all other "sampled" values are also @a@s.
sampleUncert :: Uncert a -> Corr s a (CVar s a)
sampleUncert u = liftF $ Gen u id
{-# INLINE sampleUncert #-}

-- | Generate an exact sample in 'Corr' with zero uncertainty,
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
--
-- Note that you can exactly sample an @a@ within a @'Corr' s a@, meaning
-- that all other "sampled" values are also @a@s.
sampleExact :: a -> Corr s a (CVar s a)
sampleExact = return . constC
{-# INLINE sampleExact #-}

-- | "Resolve" an 'Uncert' from a 'CVar' using its potential multiple
-- samples and sample sources, taking into account inter-correlations
-- between 'CVar's and samples.
--
-- Note that if you use 'sampleUncert' on the result, the new sample will
-- be treated as something completely independent.  Usually this should
-- only be used as the "exit point" of a 'Corr' description.
resolveUncert :: CVar s a -> Corr s a (Uncert a)
resolveUncert v = liftF $ Rei v id
{-# INLINE resolveUncert #-}
