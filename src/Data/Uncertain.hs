{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Uncertain where

import Data.Data
import Data.Foldable
import Data.Hoples
import Data.Profunctor
import Data.Reflection
import GHC.Generics
import Numeric.AD
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse     (Reverse)
import Numeric.AD.Rank1.Forward    (Forward)

data Uncert a = Un { uMean :: a
                   , uVar  :: a     -- ^ maintained to be positive
                                    --   by only exporting the :+/-
                                    --   constructor
                   }
  deriving (Data, Typeable, Generic, Generic1)

certain :: Num a => a -> Uncert a
certain x = Un x 0

infixl 6 :+/-

-- (+/-) :: Num a => a -> a -> Uncert a
-- x +/- dx = Un x (dx*dx)

pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)

uStd :: Floating a => Uncert a -> a
uStd = sqrt . uVar

asRange
    :: (Profunctor p, Functor f, Floating a)
    => p (a, a) (f (a, a))
    -> p (Uncert a) (f (Uncert a))
asRange = dimap       (\(x :+/- dx) -> (x, dx)  )
                (fmap (\(x, dx)     -> x :+/- dx))

withPrecisionAtBase :: (Floating a, RealFrac a) => Int -> a -> Int -> Uncert a
withPrecisionAtBase b x p = x' :+/- dx'
  where
    leading :: Int
    leading = negate . floor . logBase (fromIntegral b) $ x
    uncert  :: Int
    uncert  = leading - 1 + fromIntegral p
    rounder = fromIntegral b ** fromIntegral uncert
    x'      = (/ rounder) . fromIntegral . round' . (* rounder) $ x
    dx'     = 1 / rounder
    round'  :: RealFrac a => a -> Integer
    round'  = round

withPrecision :: (Floating a, RealFrac a) => a -> Int -> Uncert a
withPrecision = withPrecisionAtBase 10

uNormalizeToBase :: (Floating a, RealFrac a) => Int -> Uncert a -> Uncert a
uNormalizeToBase b u = x' :+/- dx'
  where
    x :+/- dx = u
    uncert    :: Int
    uncert    = negate . floor . logBase (fromIntegral b) $ dx
    rounder   = fromIntegral b ** fromIntegral uncert
    roundTo   = (/ rounder) . fromIntegral . round' . (* rounder)
    x'        = roundTo x
    dx'       = roundTo dx
    round'    :: RealFrac a => a -> Integer
    round'    = round

uNormalize :: (Floating a, RealFrac a) => Uncert a -> Uncert a
uNormalize = uNormalizeToBase 10

instance (Floating a, RealFrac a, Show a) => Show (Uncert a) where
    showsPrec d u | dx == 0   = showString "certain "
                              . showsPrec 9 x
                  | otherwise = showParen (d > 5) $
                                    showsPrec 6 x
                                  . showString " +/- "
                                  . showsPrec 6 dx
      where
        x :+/- dx = uNormalize u

liftUF
    :: (Traversable f, Num a)
    => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a)
    -> f (Uncert a)
    -> Uncert a
liftUF f us = Un y vy
  where
    xs         = uMean <$> us
    vxs        = uVar  <$> us
    (fx, dfxs) = grad' f xs
    y          = fx
    vy         = sum $ zipWith (\dfx vx -> dfx*dfx*vx)
                               (toList dfxs)
                               (toList vxs)

liftU
    :: Num a
    => (forall s. AD s (Forward a) -> AD s (Forward a))
    -> Uncert a
    -> Uncert a
liftU f (Un x vx) = Un y vy
  where
    (fx,dfx) = diff' f x
    y        = fx
    vy       = dfx*dfx * vx

liftU2
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU2 f x y = liftUF (\(H2 x' y') -> f x' y') (H2 x y)

liftU3
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU3 f x y z = liftUF (\(H3 x' y' z') -> f x' y' z') (H3 x y z)

liftU4
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU4 f x y z a = liftUF (\(H4 x' y' z' a') -> f x' y' z' a') (H4 x y z a)

liftU5
    :: Num a
    => (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a -> Reverse s a)
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU5 f x y z a b = liftUF (\(H5 x' y' z' a' b') -> f x' y' z' a' b') (H5 x y z a b)

instance Num a => Num (Uncert a) where
    (+)    = liftU2 (+)
    (*)    = liftU2 (*)
    (-)    = liftU2 (-)
    negate = liftU negate
    abs    = liftU abs
    signum = liftU signum
    fromInteger = certain . fromInteger

instance Fractional a => Fractional (Uncert a) where
    recip = liftU recip
    (/)   = liftU2 (/)
    fromRational = certain . fromRational

instance Floating a => Floating (Uncert a) where
    pi      = certain pi
    exp     = liftU exp
    log     = liftU log
    sqrt    = liftU sqrt
    (**)    = liftU2 (**)
    logBase = liftU2 logBase
    sin     = liftU sin
    cos     = liftU cos
    asin    = liftU asin
    acos    = liftU acos
    atan    = liftU atan
    sinh    = liftU sinh
    cosh    = liftU cosh
    asinh   = liftU asinh
    acosh   = liftU acosh
    atanh   = liftU atanh

