{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_HADDOCK hide                #-}
{-# OPTIONS_HADDOCK prune               #-}

-- |
-- Module      : Numeric.Uncertain.Correlated.Internal
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal utility functions for functionality shared by
-- "Numeric.Uncertain.Correlated" and
-- "Numeric.Uncertain.Correlated.Interactive".
--

module Numeric.Uncertain.Correlated.Internal
  ( CVar, dephantom
  , CorrF(..), Corr
  , liftCF
  , constC, liftC, liftC2, liftC3, liftC4, liftC5
  , corrToState
  )
  where

import           Control.Arrow             ((***))
import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Prelude.Compat
import           Data.Hople
import           Numeric.Uncertain
import           Numeric.AD.Mode.Sparse
import qualified Data.IntMap.Strict        as M


-- | Represents a single sample (or a value calculated from samples) within
-- the 'Corr' monad.  These can be created with 'sampleUncert',
-- 'sampleExact', and 'constC', or made by combinining others with its
-- numeric typeclass instances (like 'Num') or its functions lifting
-- arbitrary numeric functions (like 'liftC2').  These keep track of
-- inter-correlations between sources, and if you add together two 'CVar's
-- that are correlated, their results will reflect this.
--
-- Can be "resolved" into the uncertain value they represent using
-- 'resolveUncert'.
--
-- Note that these are parameterized by a dummy phantom parameter 's' so
-- that they can't be "evaluated" out of the 'Corr' they live in with
-- 'evalCorr'.
--
-- Note that a @'CVar' s a@ can only ever meaningfully "exist" in a @'Corr'
-- s a@, meaning that the all samples within that 'Corr' are of the same
-- type.
data CVar s a where
    CK :: a -> CVar s a
    CV :: M.Key -> CVar s a
    CF :: Functor f
       => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
       -> f (CVar s a)
       -> CVar s a

-- | Unsafe function to bypass the universal qualification guard for
-- returning 'CVar's from 'Corr's.
dephantom :: CVar s a -> CVar t a
dephantom = \case CK x    -> CK x
                  CV k    -> CV k
                  CF f xs -> CF f (dephantom <$> xs)

data CorrF :: * -> * -> * -> * where
    Gen :: Uncert a -> (CVar s a -> b) -> CorrF s a b
    Fun :: Functor f
        => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
        -> f (CVar s a)
        -> (CVar s a -> b)
        -> CorrF s a b
    Rei :: CVar s a
        -> (Uncert a -> b)
        -> CorrF s a b

instance Functor (CorrF s a) where
    fmap f = \case Gen u    next -> Gen u    (f . next)
                   Fun g us next -> Fun g us (f . next)
                   Rei v    next -> Rei v    (f . next)


-- | The 'Corr' monad allows us to keep track of correlated and
-- non-independent samples.  It fixes a basic "failure" of the 'Uncert'
-- type, which can't describe correlated samples.
--
-- For example, consider the difference between:
--
-- @
-- ghci> sum $ replicate 10 (12.5 '+/-' 0.8)
-- 125 +/- 3
-- ghci> 10 * (12.5 +/- 0.8)
-- 125 +/- 8
-- @
--
-- The first one represents the addition of ten independent samples, whose
-- errors will in general cancel eachother out.   The second one represents
-- sampling once and multiplying it by ten, which will amplify any error by
-- a full factor of 10.
--
-- See how the 'Corr' monad expresses the above computations:
--
-- @
-- ghci> 'evalCorr' $ do
--         x  <- 'sampleUncert' $ 12.5 '+/-' 0.8
--         y1 <- 'resolveUncert' $ sum (replicate 10 x)
--         y2 <- resolveUncert $ 10 * x
--         return (y1, y2)
-- (125 +\/- 8, 125 +\/- 8)
--
-- ghci> 'evalCorr' $ do
--         xs <- replicateM 10 ('sampleUncert' (12.5 +/- 0.8))
--         'resolveUncert' $ sum xs
-- 125 +/- 3
-- @
--
-- The first example samples once and describes operations on the single
-- sample; the second example samples 10 times with 'replicateM' and sums
-- all of the results.
--
-- Things are more interesting when you sample multiple variables:
--
-- @
-- ghci> 'evalCorr' $ do
--         x <- 'sampleUncert' $ 12.5 '+/-' 0.8
--         y <- sampleUncert $ 15.9 +/- 0.5
--         z <- sampleUncert $ 1.52 +/- 0.07
--         let k = y ** x
--         'resolveUncert' $ (x+z) * logBase z k
-- 1200 +/- 200
-- @
--
-- The first parameter is a dummy phantom parameter used to prevent 'CVar's
-- from leaking out of the computation (see 'evalCorr').  The second
-- parameter is the numeric type of all samples within the description (for
-- example, if you ever sample an @'Uncert' 'Double'@, the second parameter wil
-- be 'Double').  The third parameter is the result type of the
-- computation -- the value the 'Corr' is describing.
newtype Corr s a b = Corr { corrFree :: Free (CorrF s a) b
                          }
                   deriving (Functor, Applicative, Monad)

deriving instance MonadFree (CorrF s a) (Corr s a)

corrToState
    :: (Monad m, Fractional a)
    => Corr s a b
    -> StateT (M.Key, M.IntMap (Uncert a)) m b
corrToState = iterM go . corrFree
  where
    go = \case
            Gen u next    -> do
              i <- gets fst
              modify $ succ *** M.insert i u
              next (CV i)
            Fun f us next ->
              next $ CF f us
            Rei v next    -> do
              u <- gets (getCVar v . snd)
              next u
    getCVar
        :: forall a s. Fractional a
        => CVar s a
        -> M.IntMap (Uncert a)
        -> Uncert a
    getCVar cv = liftUF (cVarToF cv)
      where
        cVarToF
            :: CVar s a
            -> (forall t. M.IntMap (AD t (Sparse a)) -> AD t (Sparse a))
        cVarToF (CK x)    _  = auto x
        cVarToF (CV k)    us = us M.! k
        cVarToF (CF f cs) us = f (flip cVarToF us <$> cs)
{-# INLINABLE corrToState #-}

-- | Lifts a multivariate numeric function on a container (given as an @f
-- a -> a@) to work on a container of 'CVar's.  Correctly propagates the
-- uncertainty according to the second-order (multivariate) taylor
-- expansion of the function, and properly takes into account and keeps
-- track of all inter-correlations between the 'CVar' samples.  Note that
-- if the higher-degree taylor series terms are large with respect to the
-- means and variances, this approximation may be inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like '*', 'sqrt', 'atan2', etc.
--
-- @
-- ghci> 'evalCorr' $ do
--         x <- 'sampleUncert' $ 12.5 '+/-' 0.8
--         y <- sampleUncert $ 15.9 +/- 0.5
--         z <- sampleUncert $ 1.52 +/- 0.07
--         'resolveUncert' $ liftCF (\\[a,b,c] -> (a+c) * logBase c (b**a)) x y z
-- 1200 +/- 200
-- @
--
liftCF
    :: (Functor f, Fractional a)
    => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a)) -- ^ Function on container of values to lift
    -> f (CVar s a)     -- ^ Container of 'CVar' samples to apply the function to
    -> CVar s a
liftCF f cs = CF f cs
{-# INLINE liftCF #-}

-- | Creates a 'CVar' representing a completely independent sample from all
-- other 'CVar's containing the exact value given.
constC :: a -> CVar s a
constC = CK
{-# INLINE constC #-}

-- | Lifts a numeric function over the sample represented by a 'CVar'.
-- Correctly propagates the uncertainty according to the second-order
-- taylor expansion expansion of the function.  Note that if the
-- higher-degree taylor series terms are large with respect to the mean and
-- variance, this approximation may be inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like 'sqrt', 'sin', 'negate', etc.
--
-- @
-- ghci> 'evalCorr' $ do
--         x <- 'sampleUncert' $ 12.5 '+/-' 0.8
--         y <- sampleUncert $ 15.9 +/- 0.5
--         'resolveUncert' $ liftC (\\z -> log z ^ 2) (x + y)
-- 11.2 +/- 0.2
-- @
--
liftC
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a)) -- ^ Function on values to lift
    -> CVar s a         -- ^ 'CVar' sample to apply the function to
    -> CVar s a
liftC f = curryH1 $ liftCF (uncurryH1 f)
{-# INLINABLE liftC #-}

-- | Lifts a two-argument (curried) function over the samples represented
-- by two 'CVar's.  Correctly propagates the uncertainty according to the
-- second-order (multivariate) taylor expansion expansion of the function,
-- and properly takes into account and keeps track of all
-- inter-correlations between the 'CVar' samples.  Note that if the
-- higher-degree taylor series terms are large with respect to the mean and
-- variance, this approximation may be inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like '*', 'atan2', '**', etc.
--
-- @
-- ghci> 'evalCorr' $ do
--         x <- 'sampleUncert' $ 12.5 '+/-' 0.8
--         y <- sampleUncert $ 15.9 +/- 0.5
--         'resolveUncert' $ liftC2 (\\a b -> log (a + b) ^ 2) x y
-- 11.2 +/- 0.2
-- @
--
liftC2
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC2 f = curryH2 $ liftCF (uncurryH2 f)
{-# INLINABLE liftC2 #-}

-- | Lifts a three-argument (curried) function over the samples represented
-- by three 'CVar's.  See 'liftC2' and 'liftCF' for more details.
liftC3
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC3 f = curryH3 $ liftCF (uncurryH3 f)
{-# INLINABLE liftC3 #-}

-- | Lifts a four-argument (curried) function over the samples represented
-- by four 'CVar's.  See 'liftC2' and 'liftCF' for more details.
liftC4
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC4 f = curryH4 $ liftCF (uncurryH4 f)
{-# INLINABLE liftC4 #-}

-- | Lifts a five-argument (curried) function over the samples represented
-- by five 'CVar's.  See 'liftC2' and 'liftCF' for more details.
liftC5
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC5 f = curryH5 $ liftCF (uncurryH5 f)
{-# INLINABLE liftC5 #-}

instance Fractional a => Num (CVar s a) where
    (+)    = liftC2 (+)
    {-# INLINE (+) #-}
    (*)    = liftC2 (*)
    {-# INLINE (*) #-}
    (-)    = liftC2 (-)
    {-# INLINE (-) #-}
    negate = liftC negate
    {-# INLINE negate #-}
    abs    = liftC abs
    {-# INLINE abs #-}
    signum = liftC signum
    {-# INLINE signum #-}
    fromInteger = constC . fromInteger
    {-# INLINE fromInteger #-}

instance Fractional a => Fractional (CVar s a) where
    recip = liftC recip
    {-# INLINE recip #-}
    (/)   = liftC2 (/)
    {-# INLINE (/) #-}
    fromRational = constC . fromRational
    {-# INLINE fromRational #-}

instance Floating a => Floating (CVar s a) where
    pi      = constC pi
    {-# INLINE pi #-}
    exp     = liftC exp
    {-# INLINE exp #-}
    log     = liftC log
    {-# INLINE log #-}
    sqrt    = liftC sqrt
    {-# INLINE sqrt #-}
    (**)    = liftC2 (**)
    {-# INLINE (**) #-}
    logBase = liftC2 logBase
    {-# INLINE logBase #-}
    sin     = liftC sin
    {-# INLINE sin #-}
    cos     = liftC cos
    {-# INLINE cos #-}
    asin    = liftC asin
    {-# INLINE asin #-}
    acos    = liftC acos
    {-# INLINE acos #-}
    atan    = liftC atan
    {-# INLINE atan #-}
    sinh    = liftC sinh
    {-# INLINE sinh #-}
    cosh    = liftC cosh
    {-# INLINE cosh #-}
    asinh   = liftC asinh
    {-# INLINE asinh #-}
    acosh   = liftC acosh
    {-# INLINE acosh #-}
    atanh   = liftC atanh
    {-# INLINE atanh #-}
