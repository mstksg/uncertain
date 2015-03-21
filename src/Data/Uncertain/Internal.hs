{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DeriveGeneric #-}
#endif

module Data.Uncertain.Internal (
    Uncertain(..)
  , uNormalize
  , certain
  , uVal
  , uVar
  , uComponents
  ) where

import Data.Data
import Prelude hiding ((^))
import Control.Arrow ((&&&))
import qualified Prelude ((^))
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
import GHC.Generics
#endif

-- | A data type representing a number with an associated
-- experimental/measurement uncertainty.
--
-- So that you can treat it like a normal number, it is an instance of
-- 'Num', 'Fractional', 'Floating', and 'Real', 'RealFrac', and
-- 'RealFloat'.
--
-- You can apply any function or combination/composition of functions from
-- any of the above typeclasses.
--
-- All typeclasses are fully defined, so go crazy!  You should technically
-- be able to do anything you can possibly do with a generic instance of
-- 'Num', 'Floating', etc.
--
-- Some instance notes:
--
--  * The 'Show' instance displays the "normalized" value (see
--  'uNormalize').
--
--  * The 'Eq' and 'Ord' instances simply compare the central/most likely
--  values.
--
--  * For 'RealFrac' and 'RealFloat' instances, most functions are
--  defined to be from the central value/most likely value.  However,
--  'ceiling' and 'floor' are taken to include the "entire range":
--
--  > > ceiling (7.8 +/- 0.4)
--  > 9             -- as if calling `ceiling 8.2`
--  > > floor (14.1 +/- 0.3)
--  > 12            -- as if calling `floor 13.8`
data Uncertain a = !a :+- !a deriving (
                    Data
                 , Typeable
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
                 , Generic
#if __GLASGOW_HASKELL__ >= 706
                 , Generic1
#endif
#endif
                 )

-- | Displays the "normalized" value.
instance (Show a, RealFloat a, Eq a) => Show (Uncertain a) where
    showsPrec d (x :+- dx) | dx == 0   = showString "certain " .
                                         showsPrec (up_prec+1) x
                           | otherwise = showParen (d > up_prec) $
                                            showsPrec (up_prec+1) x' .
                                            showString " +/- "       .
                                            clean                    .
                                            showsPrec (up_prec+1) dx'
      where
        up_prec      = 5
        (x' :+- dx') = uNormalize (x :+- dx)
        clean ('.':'0':'e':cs) = '.' : 'e' : clean cs
        clean ('0':'.':'0':[]) = "0"
        clean ('.':'0':[])     = "."
        clean (c:cs)           = c   :       clean cs
        clean []               = []

instance Floating a => Num (Uncertain a) where
    (x :+- dx) + (y :+- dy) = (x + y) :+- sqrt (dx^2 + dy^2)
    (x :+- dx) * (y :+- dy) = (x * y) :+- sqrt ((y*dx)^2 + (x*dy)^2)
    (x :+- dx) - (y :+- dy) = (x - y) :+- sqrt (dx^2 + dy^2)
    negate (x :+- dx)       = negate x :+- dx
    abs    (x :+- dx)       = abs x    :+- dx
    signum (x :+- dx)       = signum x :+- dx           -- really?
    fromInteger             = certain . fromInteger

instance Floating a => Fractional (Uncertain a) where
    (x :+- dx) / (y :+- dy) = (x / y) :+- sqrt ((dx/y)^2 + (dy*x/y^2)^2)
    fromRational            = certain . fromRational

instance Floating a => Floating (Uncertain a) where
    pi               = certain pi
    exp  (x :+- dx)  = exp x  :+- (exp x * dx)
    sqrt (x :+- dx)  = sqrt x :+- (dx/(2 * sqrt x))
    log  (x :+- dx)  = log x  :+- (dx/x)
    sin  (x :+- dx)  = sin x  :+- (abs (cos x) * dx)
    cos  (x :+- dx)  = cos x  :+- (abs (sin x) * dx)
    asin (x :+- dx)  = asin x :+- (dx / sqrt (1 - x^2))
    atan (x :+- dx)  = atan x :+- (dx / (x^2 + 1))
    acos (x :+- dx)  = acos x :+- (dx / sqrt (1 - x^2))
    sinh (x :+- dx)  = sinh x :+- (cosh x * dx)
    tanh (x :+- dx)  = tanh x :+- (dx / cosh x ^ 2)
    cosh (x :+- dx)  = cosh x :+- (sinh x * dx)
    asinh (x :+- dx) = asinh x :+- (dx / sqrt (x^2 + 1))
    atanh (x :+- dx) = atanh x :+- (dx / abs (1 - x^2))
    acosh (x :+- dx) = acosh x :+- (dx / sqrt (x^2 - 1))
    (x :+- dx) ** (y :+- dy)      = (x ** y) :+-
                                      sqrt ( (y * x ** (y-1) * dx)^2
                                           + (x ** y * log x * dy)^2
                                           )
    logBase (b :+- db) (x :+- dx) = logBase b x :+-
                                      sqrt ( (db * log x / (b * log x ^2))^2
                                           + (dx / (x * log b))^2
                                           )

instance (Real a, Fractional a, Floating a) => Real (Uncertain a) where
    toRational (x :+- _) = toRational x

-- | See note on 'ceiling' and 'floor'.
instance (Floating a, RealFrac a) => RealFrac (Uncertain a) where
    properFraction (x :+- dx) = let (j,k) = properFraction x in (j, k :+- dx)
    truncate                  = truncate . uVal
    round                     = round    . uVal
    ceiling                   = ceiling  . uncurry (+) . uComponents
    floor                     = floor    . uncurry (-) . uComponents

-- | Most of these work on the central/most likely value.
instance (RealFloat a) => RealFloat (Uncertain a) where
    floatRadix              = floatRadix     . uVal
    floatDigits             = floatDigits    . uVal
    floatRange              = floatRange     . uVal
    decodeFloat             = decodeFloat    . uVal
    exponent                = exponent       . uVal
    significand             = certain        . significand . uVal
    isNaN                   = isNaN          . uVal
    isInfinite              = isInfinite     . uVal
    isDenormalized          = isDenormalized . uVal
    isNegativeZero          = isNegativeZero . uVal
    isIEEE                  = isIEEE         . uVal
    encodeFloat x y         = certain (encodeFloat x y)
    scaleFloat n (x :+- dx) = scaleFloat n x :+- dx
    atan2 y x               = atan2 (uVal y) (uVal x) :+- uVar (atan (y/x))

-- | Compares the central/most likely values.
instance Eq a => Eq (Uncertain a) where
    (x :+- _) == (y :+- _) = x == y

-- | Compares the central/most likely values.
instance Ord a => Ord (Uncertain a) where
    compare (x :+- _) (y :+- _) = compare x y

-- | "Normalize" an 'Uncertain'.
--
-- This means that the value can't have any more precision than implied by
-- the uncertainty, and that the uncertainty can't have more than one digit
-- of precision/significance.
--
-- > > uComponents $ uNormalize (15.235812734 +/- 0.0348513)
-- > (15.23, 3.0e-2)
--
-- Note that the 'Show' instance of 'Uncertain' automatically normalizes
-- before showing.
--
-- This is because, effectively, varying any of the digits that are
-- "normalized away" should yield an indistinguishable
-- 'Uncertain'/experimental result.
--
-- Has undefined behavior for "certain" values, with zero uncertainty.
uNormalize :: (RealFrac a, Floating a) => Uncertain a -> Uncertain a
uNormalize (x :+- dx) = x' :+- dx'
  where
    uncert  = negate . floor . logBase 10 $ dx :: Integer
    rounder = 10 ** fromIntegral uncert
    roundTo = (/ rounder) . fromIntegral . round' . (* rounder)
    x'      = roundTo x
    dx'     = roundTo dx



-- | Wraps a 'Num' in an 'Uncertain', associating it with an uncertainty of
-- @0@.  Represents a number you know exactly, with no uncertainty.
--
-- This is also the default 'fromInteger' method from the 'Num' instance.
--
-- > > certain 5 * certin 9
-- > certain 45.0
-- > > (18.2 +/- 0.3) ** certain (1/2)
-- > 4.271 +/- 4.e-3
--
-- Usually, this can be ignored and only the actual literal needs be
-- entered.
--
-- > > 5 * 9 :: Uncertain Double
-- > certain 45.0
-- > > (18.2 +/- 0.3) ** (1/2)
-- > 4.271 +/- 4.e-3
--
-- Use when you want an actual function.
--
-- > > map certain [1,2,3]
-- > [certain 1.0, certain 2.0, certain 3.0]
certain :: Num a => a -> Uncertain a
certain = (:+- 0)

-- | Get the (central/most likely) value out of an 'Uncertain'.
--
-- > > uVal (15.2 +/- 0.4)
-- > 15.2
uVal :: Uncertain a -> a
uVal (x :+- _) = x

-- | Get the uncertainty/root variance out of an 'Uncertain'.
--
-- > > uVar (15.2 +/- 0.4)
-- > 0.4
uVar :: Uncertain a -> a
uVar (_ :+- dx) = dx

-- | Returns the two components (the central value and the root
-- variance) of an 'Uncertain'.
--
-- > uComponents = uVal &&& uVar
uComponents :: Uncertain a -> (a, a)
uComponents = uVal &&& uVar

round' :: RealFrac a => a -> Integer
round' = round

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

