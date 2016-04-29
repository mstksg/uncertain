{-# LANGUAGE PatternSynonyms #-}

module Data.Uncertain.MonteCarlo where

import Control.Monad
import Control.Monad.Primitive
import Data.Hople
import Data.List
import Data.Traversable
import Data.Uncertain (Uncert, pattern (:+/-), withVar)
import System.Random.MWC
import System.Random.MWC.Distributions

liftU
    :: PrimMonad m
    => (Double -> Double)
    -> Uncert Double
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftU f u g = uncertStats <$> replicateM 10000 samp
  where
    samp = f <$> normal x dx g
    x :+/- dx = u

liftUF
    :: (Traversable f, PrimMonad m)
    => (f Double -> Double)
    -> f (Uncert Double)
    -> Gen (PrimState m)
    -> m (Uncert Double)
liftUF f us g = uncertStats <$> replicateM 10000 samp
  where
    samp = fmap f . for us $ \(x :+/- dx) ->
             normal x dx g

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



-- liftU4
--     :: PrimMonad m
--     => (Double -> Double -> Double -> Double -> Double)
--     -> Uncert Double
--     -> Uncert Double
--     -> Uncert Double
--     -> Uncert Double
--     -> Gen (PrimState m)
--     -> m (Uncert Double)
-- liftU4 f x y z a = liftUF (\(H3 x' y' z' a') -> f x' y' z' a') (H3 x y z a)

-- liftU5
--     :: PrimMonad m
--     => (Double -> Double -> Double -> Double -> Double -> Double)
--     -> Uncert Double
--     -> Uncert Double
--     -> Uncert Double
--     -> Uncert Double
--     -> Gen (PrimState m)
--     -> m (Uncert Double)
-- liftU5 f x y z a b = liftUF (\(H3 x' y' z' a' b') -> f x' y' z' a' b') (H3 x y z a b)


uncertStats :: Fractional a => [a] -> Uncert a
uncertStats = (withVar <$> getMean <*> getVar) . foldStat
  where
    getMean (H3 x0 x1 _ ) = x1/x0
    getVar  (H3 x0 x1 x2) = x2/x0 - (x1/x0)^(2 :: Int)
    foldStat = flip foldl' (H3 0 0 0) $
                 \(H3 s0 s1 s2) x ->
                   H3 (s0 + 1) (s1 + x) (s2 + x*x)



