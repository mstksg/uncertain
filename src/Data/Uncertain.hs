{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Uncertain
  ( Uncert
  , pattern (:+/-)
  , uMean, uVar, uStd, uMeanVar, uMeanStd, uRange
  , (+/-), exact, withPrecisionAtBase, withPrecision, withVar
  , uNormalizeAtBase, uNormalize
  , liftUF
  , liftU, liftU', liftU2, liftU3, liftU4, liftU5
  )
  where

import           Control.Arrow          ((&&&))
import           Data.Data
import           Data.Foldable
import           Data.Function
import           Data.Hople
import           Data.Ord
import           GHC.Generics
import           Numeric.AD.Mode.Sparse
import qualified Numeric.AD.Mode.Tower  as T

-- | Represents an independent experimental value centered around a mean
-- value with "inherent" and independent uncertainty.
--
-- Mostly useful due to its instances of numeric typeclasses like `Num`,
-- `Fractional`, etc., which allows you to add and multiply and apply
-- arbitrary numerical functions to them and have the uncertainty
-- propagate appropriately.
--
-- Can be created with 'exact' to represent an "exact" measurement with no
-- uncertainty, '+/-' and ':+/-' to specify a standard deviation as
-- a range, 'withPrecision' to specify through decimal precision, and
-- 'withVar' to specify with a variance.
--
-- Can be deconstructed with ':+/-', the pattern synonym/pseudo-constructor
-- which matches on the mean and a standard deviation.  You can also access
-- properties with 'uMean', 'uStd', 'uVar', 'uMeanStd', 'uMeanVar',
-- 'uRange', etc.
--
-- It's important to remember that each "occurrence" represents a unique
-- independent sample, so:
--
-- > > let x = 15 +/- 2 in x + x
-- > 30 +/- 3
-- >
-- > > let x = 15 +/- 2 in x*2
-- > 30 +/- 4
--
-- @x + x@ does not represent adding the same sample to itself twice, it
-- represents /independently/ sampling two values within the range @15 +/- 2@
-- and adding them together.  In general, errors and deviations will cancel
-- each-other out, leading to a smaller uncertainty.
--
-- However, @x*2@ represents taking /one/ sample and multiplying it by two.
-- This yields a greater uncertainty, because errors and deviations are
-- amplified.
--
-- Also be aware that the 'Show' instance "normalizes" the result, and
-- won't show any mean/central point to a decimal precision smaller than
-- the uncertainty, rounding off the excess.
--
data Uncert a = Un { _uMean :: !a
                   , _uVar  :: !a    -- ^ maintained to be positive!
                   }
  deriving (Data, Typeable, Generic, Generic1)

uMean :: Uncert a -> a
uMean = _uMean

uVar :: Uncert a -> a
uVar = _uVar

uStd :: Floating a => Uncert a -> a
uStd = sqrt . uVar

exact :: Num a => a -> Uncert a
exact x = Un x 0

infixl 6 +/-
infixl 6 :+/-

(+/-) :: Num a => a -> a -> Uncert a
x +/- dx = Un x (dx*dx)

withVar :: Num a => a -> a -> Uncert a
withVar x vx = Un x (abs vx)

pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)

uMeanVar :: Uncert a -> (a, a)
uMeanVar = uMean &&& uVar

uMeanStd :: Floating a => Uncert a -> (a, a)
uMeanStd = uMean &&& uStd

uRange :: Floating a => Uncert a -> (a, a)
uRange u = let x :+/- dx = u
           in  (x - dx, x + dx)

withPrecisionAtBase
    :: (Floating a, RealFrac a)
    => Int
    -> a
    -> Int
    -> Uncert a
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

withPrecision
    :: (Floating a, RealFrac a)
    => a
    -> Int
    -> Uncert a
withPrecision = withPrecisionAtBase 10

uNormalizeAtBase
    :: (Floating a, RealFrac a)
    => Int
    -> Uncert a
    -> Uncert a
uNormalizeAtBase b u = x' :+/- dx'
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

uNormalize
    :: (Floating a, RealFrac a)
    => Uncert a
    -> Uncert a
uNormalize = uNormalizeAtBase 10

instance (Show a, Floating a, RealFrac a) => Show (Uncert a) where
    showsPrec d = uShowsPrec' d . uNormalize

uShowsPrec' :: (Show a, Floating a) => Int -> Uncert a -> ShowS
uShowsPrec' d u = showParen (d > 5) $
                      showsPrec 6 x
                    . showString " +/- "
                    . showsPrec 6 dx
  where
    x :+/- dx = u

liftUF
    :: (Traversable f, Fractional a)
    => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a))
    -> f (Uncert a)
    -> Uncert a
liftUF f us = Un y vy
  where
    xs          = uMean <$> us
    vxs         = uVar  <$> us
    vxsL        = toList vxs
    (fx, dfxsh) = hessian' f xs
    dfxs        = fst <$> dfxsh
    hess        = snd <$> dfxsh
    y           = fx + hessTerm / 2
      where
        hessTerm = sum . zipWith (*) vxsL . toList
                 . fmap (sum . zipWith (*) vxsL . toList)
                 $ hess
    vy          = sum $ zipWith (\dfx vx -> dfx*dfx*vx)
                                (toList dfxs)
                                vxsL

liftU
    :: Fractional a
    => (forall s. AD s (T.Tower a) -> AD s (T.Tower a))
    -> Uncert a
    -> Uncert a
liftU f (Un x vx) = Un y vy
  where
    fx:dfx:ddfx:_ = T.diffs0 f x
    y             = fx + ddfx * vx / 2
    vy            = dfx*dfx * vx

liftU'
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
liftU' f x = liftUF (\(H1 x') -> f x') (H1 x)

liftU2
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU2 f x y = liftUF (\(H2 x' y') -> f x' y') (H2 x y)

liftU3
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU3 f x y z = liftUF (\(H3 x' y' z') -> f x' y' z') (H3 x y z)

liftU4
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU4 f x y z a = liftUF (\(H4 x' y' z' a') -> f x' y' z' a') (H4 x y z a)

liftU5
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU5 f x y z a b = liftUF (\(H5 x' y' z' a' b') -> f x' y' z' a' b') (H5 x y z a b)

instance Fractional a => Num (Uncert a) where
    (+)    = liftU2 (+)
    (*)    = liftU2 (*)
    (-)    = liftU2 (-)
    negate = liftU negate
    abs    = liftU abs
    signum = liftU signum
    fromInteger = exact . fromInteger

instance Fractional a => Fractional (Uncert a) where
    recip = liftU recip
    (/)   = liftU2 (/)
    fromRational = exact . fromRational

instance Floating a => Floating (Uncert a) where
    pi      = exact pi
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

instance Eq a => Eq (Uncert a) where
    (==) = (==) `on` uMean
    (/=) = (/=) `on` uMean

instance Ord a => Ord (Uncert a) where
    compare = comparing uMean

instance (Fractional a, Real a) => Real (Uncert a) where
    toRational = toRational . uMean

instance RealFrac a => RealFrac (Uncert a) where
    properFraction x = (n, d)
      where
        d    = liftU (snd' . properFraction) x
        n    = fst . properFraction $ uMean x
        snd' :: (Int, b) -> b
        snd' = snd
    truncate = truncate . uMean
    round    = round    . uMean
    ceiling  = ceiling  . uMean
    floor    = floor    . uMean

instance RealFloat a => RealFloat (Uncert a) where
    floatRadix      = floatRadix        . uMean
    floatDigits     = floatDigits       . uMean
    floatRange      = floatRange        . uMean
    decodeFloat     = decodeFloat       . uMean
    exponent        = exponent          . uMean
    isNaN           = isNaN             . uMean
    isInfinite      = isInfinite        . uMean
    isDenormalized  = isDenormalized    . uMean
    isNegativeZero  = isNegativeZero    . uMean
    isIEEE          = isIEEE            . uMean
    encodeFloat a b = exact (encodeFloat a b)
    significand     = liftU significand
    atan2           = liftU2 atan2
