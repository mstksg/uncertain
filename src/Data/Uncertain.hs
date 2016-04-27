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
  , uVar', uStd', uMeanVar', uMeanStd'
  , (+/-), exact, withPrecisionAtBase, withPrecision, withVar
  , uNormalizeAtBase, uNormalize
  , liftUF
  , liftU, liftU', liftU2, liftU3, liftU4, liftU5
  )
  where

import           Control.Applicative
import           Control.Arrow          ((&&&))
import           Data.Data
import           Data.Coerce
import           Data.Foldable
import           Data.Function
import           Data.Hople
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           GHC.Generics
import           Numeric.AD.Mode.Sparse
import qualified Numeric.AD.Mode.Tower  as T

data Uncert a = Un !a !a -- should be positive!
              | Ce !a
  deriving (Data, Typeable, Generic, Generic1)

uMean :: Uncert a -> a
uMean (Un x _) = x
uMean (Ce x)   = x

uVar :: Num a => Uncert a -> a
uVar = fromMaybe 0 . uVar'

uVar' :: Uncert a -> Maybe a
uVar' (Un _ v) = Just v
uVar' (Ce _)   = Nothing

uStd :: Floating a => Uncert a -> a
uStd = sqrt . uVar

uStd' :: Floating a => Uncert a -> Maybe a
uStd' = fmap sqrt . uVar'

exact :: a -> Uncert a
exact = Ce

infixl 6 :+/-

(+/-) :: Num a => a -> a -> Uncert a
x +/- dx = Un x (dx*dx)

withVar :: Num a => a -> a -> Uncert a
withVar x vx = Un x (abs vx)

pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
pattern x :+/- dx <- (uMeanStd->(x, dx))
  where
    x :+/- dx = Un x (dx*dx)

uMeanVar :: Num a => Uncert a -> (a, a)
uMeanVar = uMean &&& uVar

uMeanVar' :: Uncert a -> (a, Maybe a)
uMeanVar' = uMean &&& uVar'

uMeanStd :: Floating a => Uncert a -> (a, a)
uMeanStd = uMean &&& uStd

uMeanStd' :: Floating a => Uncert a -> (a, Maybe a)
uMeanStd' = uMean &&& uStd'

uRange :: Floating a => Uncert a -> (a, a)
uRange (Un x (sqrt->dx)) = (x - dx, x + dx)
uRange (Ce x)            = (x     , x)

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
uNormalizeAtBase _ (Ce x)            = Ce x
uNormalizeAtBase b (Un x (sqrt->dx)) = x' :+/- dx'
  where
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

instance (Floating a, RealFrac a, Show a) => Show (Uncert a) where
    showsPrec d (uNormalize->u) =
        case u of
          Ce x            -> showString "exact "
                           . showsPrec 9 x
          Un x (sqrt->dx) -> showParen (d > 5) $
                                 showsPrec 6 x
                               . showString " +/- "
                               . showsPrec 6 dx

liftUF
    :: forall f a. (Traversable f, Fractional a)
    => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a))
    -> f (Uncert a)
    -> Uncert a
liftUF f us = case vy' of
                Just vy -> Un y vy
                Nothing -> Ce y
  where
    xs          :: f a
    xs          = uMean <$> us
    vxsL        :: [Maybe a]
    vxsL        = map uVar' . toList $ us
    (fx, dfxsh) = hessian' f xs
    dfxs        :: f a
    dfxs        = fst <$> dfxsh
    hess        :: f (f a)
    hess        = snd <$> dfxsh
    y           :: a
    y           = case hessTerm of
                    Just h  -> fx + h / 2
                    Nothing -> fx
      where
        hessTerm :: Maybe a
        hessTerm = sumMaybes . zipWith (liftA2 (*)) vxsL . toList
                 . fmap (sumMaybes . zipWith (*!) vxsL . toList)
                 $ hess
    vy'         :: Maybe a
    vy'         = sumMaybes $
                    zipWith (\dfx vx -> vx *! (dfx*dfx))
                            (toList dfxs)
                            vxsL
    (*!) :: Maybe a -> a -> Maybe a
    v *! d = fmap (*d) v
    sumMaybes :: [Maybe a] -> Maybe a
    sumMaybes = (coerce :: Maybe (Sum a) -> Maybe a)
              . foldMap coerce

liftU
    :: Fractional a
    => (forall s. AD s (T.Tower a) -> AD s (T.Tower a))
    -> Uncert a
    -> Uncert a
liftU f (uMeanVar'->(x, vx')) =
    case vx' of
      Just vx -> let dfx:ddfx:_ = dfs
                     y          = fx + ddfx * vx / 2
                     vy         = dfx*dfx * vx
                 in  Un y vy
      Nothing -> Ce fx
  where
    fx:dfs = T.diffs0 f x

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
