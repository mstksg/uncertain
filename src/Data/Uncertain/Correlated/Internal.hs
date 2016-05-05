{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_HADDOCK hide                #-}

module Data.Uncertain.Correlated.Internal
  ( CVar, dephantom
  , CorrF(..), Corr
  , liftCF
  , constC, liftC, liftC2, liftC3, liftC4, liftC5
  , corrToState
  )
  where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Hople
import           Data.Uncertain
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

deriving instance Functor (CorrF s a)

-- | The 'Corr' monad allows us to keep track of correlated and
-- non-independent samples:
--
-- @
-- evalCorr $ do
--   x <- sampleUncert $ 12.5 +/- 0.8
--   y1 <- resolveUncert $ sum (replicate 10 x)
--   y2 <- resolveUncert $ 10 * x
--   return (y1, y2)
--   -- result: (125 +/- 8, 125 +/- 8)
-- 
-- evalCorr $ do
--   xs <- replicateM 10 (sampleUncert (12.5 +/- 0.8))
--   resolveUncert $ sum xs
--   -- result: 125 +/- 3
-- @
--
-- The first example samples once and describes operations on the single
-- sample; the second example samples 10 times with 'replicateM' and sums
-- all of the results.
--
-- Things are more interesting when you sample multiple variables:
--
-- @
-- evalCorr $ do
--   x <- sampleUncert $ 12.5 +/- 0.8
--   y <- sampleUncert $ 15.9 +/- 0.5
--   z <- sampleUncert $ 1.52 +/- 0.07
--   resolveUncert $ (x+z)*logBase z (y**x)
--   -- result: 1200 +/- 200
-- @
--
-- The first parameter is a dummy phantom parameter used to prevent 'CVar's
-- from leaking out of the computation (see 'evalCorr').  The second
-- parameter is the numeric type of all samples within the description (for
-- example, if you ever sample an 'Uncert Double', the second parameter wil
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
              modify $ bimap succ (M.insert i u)
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
-- evalCorr $ liftCF (\[x,y,z] -> x*y+z) [12.2 +/- 0.5, 56 +/- 2, 0.12 +/- 0.08]
-- 680 +/- 40
-- @
--
liftCF
    :: (Functor f, Fractional a)
    => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
    -> f (CVar s a)
    -> CVar s a
liftCF f cs = CF f cs

-- | Creates a 'CVar' representing a completely independent sample from all
-- other 'CVar's containing the exact value given.
constC :: a -> CVar s a
constC = CK

liftC
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
liftC f = curryH1 $ liftCF (uncurryH1 f)

liftC2
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC2 f = curryH2 $ liftCF (uncurryH2 f)

liftC3
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC3 f = curryH3 $ liftCF (uncurryH3 f)

liftC4
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC4 f = curryH4 $ liftCF (uncurryH4 f)

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

instance Fractional a => Num (CVar s a) where
    (+)    = liftC2 (+)
    (*)    = liftC2 (*)
    (-)    = liftC2 (-)
    negate = liftC negate
    abs    = liftC abs
    signum = liftC signum
    fromInteger = constC . fromInteger

instance Fractional a => Fractional (CVar s a) where
    recip = liftC recip
    (/)   = liftC2 (/)
    fromRational = constC . fromRational

instance Floating a => Floating (CVar s a) where
    pi      = constC pi
    exp     = liftC exp
    log     = liftC log
    sqrt    = liftC sqrt
    (**)    = liftC2 (**)
    logBase = liftC2 logBase
    sin     = liftC sin
    cos     = liftC cos
    asin    = liftC asin
    acos    = liftC acos
    atan    = liftC atan
    sinh    = liftC sinh
    cosh    = liftC cosh
    asinh   = liftC asinh
    acosh   = liftC acosh
    atanh   = liftC atanh
