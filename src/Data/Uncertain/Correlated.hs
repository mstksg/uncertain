{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Uncertain.Correlated
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
-- Fixes a basic "failure" of the 'Uncert' type, which can't describe
-- correlated samples.
--
-- For example, consider the difference between:
--
-- > sum $ replicate 10 (12.5 +/- 0.8)   -- 125 +/- 3
-- > 10 * (12.5 +/- 0.8)                 -- 125 +/- 8
--
-- The first one represents the addition of ten independent samples, whose
-- errors will in general cancel eachother out.   The second one represents
-- sampling once and multiplying it by ten, which will amplify any error by
-- a full factor of 10.
--

module Data.Uncertain.Correlated
  ( Corr, evalCorr
  , CVar
  , sampleUncert, sampleExact
  , resolveUncert
  , constC, liftC, liftC2, liftC3, liftC4, liftC5, liftCF
  )
  where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Data.Uncertain
import           Data.Uncertain.Correlated.Internal
import qualified Data.IntMap.Strict                 as M

-- | Evaluates the value described by a 'Corr' monad, taking into account
-- inter-correlations between samples.
--
-- Takes a universally qualified 'Corr', which should not affect usage.
-- See the examples in the documentation for 'Corr'.  The univeral
-- qualification is mostly a type system trick to ensure that you aren't
-- allowed to ever use 'evalCorr' to evaluate a 'CVar'.
evalCorr :: Fractional a => (forall s. Corr s a b) -> b
evalCorr c = evalState (corrToState c) (0, M.empty)

-- | Generate a sample in 'Corr' from an 'Uncert' value, independently from
-- all other samples.
--
-- Note that you can only sample @'Uncert' a@s within a @'Corr' s a@, meaning
-- that all other "sampled" values are also @a@s.
sampleUncert :: Uncert a -> Corr s a (CVar s a)
sampleUncert u = liftF $ Gen u id

-- | Generate an exact sample in 'Corr' with zero uncertainty,
-- independently from all other samples.
--
-- Not super useful, since you can do something equivalent with 'constC'
-- or the numeric instances:
--
-- > sampleExact x  ≡ return ('constC' x)
-- > sampleExact 10 ≡ return 10
--
-- But is provided for completeness alongside 'sampleUncert'.
--
-- Note that you can exactly sample an @a@ within a @'Corr' s a@, meaning
-- that all other "sampled" values are also @a@s.
--
sampleExact :: a -> Corr s a (CVar s a)
sampleExact = return . constC

-- | "Resolve" an 'Uncert' from a 'CVar' using its potential multiple
-- samples and sample sources, taking into account inter-correlations
-- between 'CVar's and samples.
--
-- Note that if you use 'sampleUncert' on the result, the new sample will
-- be treated as something completely independent.  Usually this should
-- only be used as the "exit point" of a 'Corr' description.
resolveUncert :: CVar s a -> Corr s a (Uncert a)
resolveUncert v = liftF $ Rei v id

