{-# LANGUAGE PatternSynonyms #-}

module Data.Uncertain.MonteCarlo
  ( liftUF
  , liftU, liftU2, liftU3, liftU4, liftU5
  , sampleUncert
  )
  where

import Control.Monad
import Control.Monad.Primitive
import Data.Hople
import Data.Uncertain (Uncert, pattern (:+/-), fromSamples)
import System.Random.MWC
import System.Random.MWC.Distributions

sampleUncert
    :: PrimMonad m
    => Uncert Double
    -> Gen (PrimState m)
    -> m Double
sampleUncert u g = normal x dx g
  where
    x :+/- dx = u

liftU
    :: PrimMonad m
    => (Double -> Double)
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU f u g = fromSamples <$> replicateM 10000 samp
  where
    samp = f <$> sampleUncert u g

liftUF
    :: (Traversable f, PrimMonad m)
    => (f Double -> Double)
    -> f (Uncert Double)
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftUF f us g = fromSamples <$> replicateM 10000 samp
  where
    samp = f <$> traverse (flip sampleUncert g) us

liftU2
    :: PrimMonad m
    => (Double -> Double -> Double)
    -> Uncert Double
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU2 f x y = liftUF (uncurryH2 f) (H2 x y)

liftU3
    :: PrimMonad m
    => (Double -> Double -> Double -> Double)
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU3 f x y z = liftUF (uncurryH3 f) (H3 x y z)

liftU4
    :: PrimMonad m
    => (Double -> Double -> Double -> Double -> Double)
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU4 f x y z a = liftUF (uncurryH4 f) (H4 x y z a)

liftU5
    :: PrimMonad m
    => (Double -> Double -> Double -> Double -> Double -> Double)
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU5 f x y z a b = liftUF (uncurryH5 f) (H5 x y z a b)

