{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Data.Uncertain
-- Copyright   : (c) Justin Le 2014
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- This module provides the 'Uncertain a' type, which represents a number
-- with associated measurement/experimental uncertainty.  So that you can
-- treat it like a normal number, it also provides 'Num', 'Fractional',
-- 'Floating', and 'Real', 'RealFrac', and 'RealFloat' instances, along
-- with all associated functions ('exp', '(**)', 'sin', '(/)', etc.), which
-- properly propagate uncertainty with principles from statistics.
--
-- Because of this, all generic functions designed to work for all 'Num'
-- (or 'Floating', 'Real', etc.) will work, and propagate your uncertainty.
--
-- You can directly construct these numbers with either '(+/-)' or
-- 'certain'.  'withPrecision' is also provided as a helper function; you
-- can also directly type numeric literals and the 'Num' and 'Fractional'
-- instances will automatically convert them.
--
-- > 7.13 +/- 0.05
-- > 91800 +/- 100
-- > certain 7.9512
-- > 81.42 `withPrecision` 4
-- > 7    :: Uncertain Double
-- > 9.18 :: Uncertain Double
--
-- Now you can do anything!
--
-- > > let x = 1.52 +/- 0.07
-- > > let y = 781.4 +/- 0.3
-- > > let z = 1.53e-1 `withPrecision` 3
-- > > cosh x
-- > 2.4 +/- 0.2
-- > > exp x / z * sin (y ** z)
-- > 10.8 +/- 0.9
-- > > pi + 3 * logBase x y
-- > 50.87 +/- 2.e-2
--
-- If you only have "normal" functions, like @Double -> Double -> Double@,
-- you can use 'uMap', 'uMap2', and 'uMap3'.
--
-- > > let f :: Double -> Double -> Double -> Double;
-- >           f a b c = b ** exp (c / (2 * a))
-- > > y ** (exp z / (x * 2))
-- > 13.0 +/- 2.
-- > > uMap3 f x y z
-- > 13.0 +/- 2.
--
-- These use an epsilon of 0.001 to approximate the first partial
-- derivatives used in propagating the uncertainty.  If your function
-- varies very rapidly at the points in question, you may adjust this as
-- needed using 'uMap'', 'uMap2'', and 'uMap3'', which accept an @epsilon@
-- parameter.
--
-- > > sin (2^10 * z)
-- > -0.4 +/- 0.9
-- > > uMap (\a -> sin (2^10 * a)) z
-- > -0.4 +/- 0.8
-- > > uMap' 0.0001 (\a -> sin (2^10 * a)) z
-- > -0.4 +/- 0.9
--
-- There are also various utility functions to extract components out of an
-- 'Uncertain'; also, to "normalize" them to remove "virtual precision".
--
module Data.Uncertain (
    -- * The Math
    -- $math
    -- * * Gotchas
    -- $gotchas
    -- * The type
    Uncertain       -- abstract, instances: Show, Eq, Ord, Generic, Num, Fractional, Floating, Real, RealFrac, RealFloat
    -- * Constructing uncertain values
  , certain         -- :: Num a => a -> Uncertain a
  , (+/-)           -- :: Num a => a -> a -> Uncertain a
  , withPrecision   -- :: (RealFrac a, Floating a) => a -> Int -> Uncertain a
    -- * Extracting and manipulating 'Uncertain'
  , uVal            -- :: Uncertain a -> a
  , uVar            -- :: Uncertain a -> a
  , uComponents     -- :: Uncertain a -> (a, a)
  , uNormalize      -- :: (RealFrac a, Floating a) => Uncertain a -> Uncertain a
    -- * Utility functions
  , (*~)            -- :: Floating a => Int -> Uncertain a -> Uncertain a
  , (~*)            -- :: Floating a => Uncertain a -> Int -> Uncertain a
    -- * Applying numerical functions
    -- $apply
  , uMap            -- :: (Fractional a, Real a, Floating b) => (a -> b) -> Uncertain a -> Uncertain b
  , uMap2           -- :: (Fractional a, Real a, Floating b) => (a -> a -> b) -> Uncertain a -> Uncertain a -> Uncertain b
  , uMap3           -- :: (Fractional a, Real a, Floating b) => (a -> a -> a -> b) -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
    -- * Applying numerical functions (adjustable epsilon)
    -- $applyeps
  , uMap'           -- :: (Fractional a, Real a, Floating b) => a -> (a -> b) -> Uncertain a -> Uncertain b
  , uMap2'          -- :: (Fractional a, Real a, Floating b) => a -> (a -> a -> b) -> Uncertain a -> Uncertain a -> Uncertain b
  , uMap3'          -- :: (Fractional a, Real a, Floating b) => a -> (a -> a -> a -> b) -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
  ) where

import Control.Arrow
import Prelude hiding    ((^))
import qualified Prelude ((^))
import GHC.Generics

-- $math
--
-- So the math behind the propagation of uncertainty makes the assumption
-- that all experimental and measurement errors are ultimately caused by
-- the sum of independent, random, smaller contributions  Thanks to the
-- central limit theorem, this means that we can model experimental values
-- as normal distributions with a mean representing the "central/most
-- likely value" and a variance (sigma squared) representing the combined
-- magnitude of the small random indepdent errors.
--
-- If you have, say, 7 +/- 2, and 4 +/- 1, what should we expect the sum to
-- be?  Well, the new central value is going to be 11, but what is the new
-- sigma/root variance?  Is it 2 + 1 = 3?  11 +/- 3?
--
-- This actually overestimates the new variance, because while getting 5 on
-- the 7 +/- 2 value is "as common" as 3 on the 4 +/- 1 value, getting an
-- 8 on the sum is comparatively pretty rare --- it only happens when both
-- the first and the second measurements are at their minimum.  In reality,
-- when you measure a lower number for the first value, you might measure
-- a higher number or a lower number on the second.  If you measure
-- a really low number for the first value, you are pretty unlikely to also
-- measure a really low number for the second.
--
-- Basically, saying that the sum is 11 +/- 3 is assuming that the two
-- numbers have some covariance --- that their small random errors aren't
-- actually independent.
--
-- So what is the actual uncertainty of the sum?  Well, we need to take
-- into account the unlikeliness of both values being at their extremes at
-- the same time.  But if we remember from before, we can model the values
-- as normal distributions...and we know exactly how normal distributions
-- add together!  Their centers add, and their variances add --- that is,
-- the new sigma^2 = sum of the old sigma^2.  In our case, our sigmas are
-- 2 and 1, and our sigma square are 4 and 1.  So the variance of the sum
-- is the square root of 5, 2.24.  So 7 +/- 2 + 4 +/- 1 = 11 +/- 2.24.
--
-- > > uComponents $ (7 +/- 2) + (4 +/- 1)
-- > (11.0, 2.24)
--
-- Note that the default 'Show' instances only shows one digit of the
-- uncertainty:
--
-- > > (7 +/- 2) + (4 +/- 1)
-- > 11.0 +/- 2.
--
-- Surely enough, if you did find measurable things with those given values
-- and uncertainties, you will find out that this checks out --- the sum of
-- every time you measure both of them will be around 11 +/- 2, not 11 +/-
-- 3, like you'd expect.  You can even test this with a program --- make
-- two values which are sampled from a normal distribution, and add up each
-- sample!
--
-- Anyway, we can now use this to find a general formula for propagating
-- uncertainty, based on well known properties of the normal distribution:
--
-- > f (x +/- dx) (y +/- dy) (z +/- dz) =
-- >     f x y z +/- sqrt ((dfdx * dx)^2 + (dfdy * dy)^2 + (dfdz * dz)^2)
--
-- etc. etc., for arbitrary number of variables.  @dfdx@ represents the
-- partial derivative of the function @f@ with respect to @x@, at point
-- @x@.
--
-- This library provides implementations for all generic functions on
-- 'Num', 'Fractional', 'RealFrac', etc. that propagate uncertainty in this
-- way.

-- $gotchas
--
-- There are a couple things to watch out for that might be unexpected.
--
-- First of all, in general, @n * x /= x + x + x ...@ (@n@ times).
--
-- For example:
--
-- > > let x = 16.3 +/- 0.4
-- > > 3 * x
-- > 49.0 +/- 1.
-- > > x + x + x
-- > 48.9 +/- 0.7
--
-- This is desired behavior, but might surprise people at first.
--
-- Think of it this way --- if you measure something once and multiply it
-- by three, you get something different than if you measure something
-- three times (each time with different results) and add them together.
-- In the former case, if your original measurement was unusually low, your
-- product will be unusually low.  In the latter, if the first measurement
-- is unusually low, the second and third probably won't be, and it'll all
-- eventually "even out".  Hence, @x + x + x@ has less variance than @3
-- * x@.
--
-- In the same token, @y ** n /= y * y * y ...@ (@n@ times).
--
-- > > x ** 3
-- > 4300.0 +/- 300
-- > > x * x * x
-- > 4300.0 +/- 200
-- > > x^3
-- > 4300.0 +/- 200
--
-- (Note that @x^3@ is just defined as @x * x * x@)
--
-- Just some things to be careful about.  If you want to, say, measure the
-- thickness of a 500 page book where very page is 0.13 mm +/- 0.02 mm:
--
-- > > sum (replicate 500 (0.13 +/- 0.02))
-- > 65.0 +/- 0.4
--
-- But if you wanted to, say, find the circumference of a circle of
-- diameter 8.1 +/- 0.2 cm:
--
-- > > pi * (8.1 +/- 0.2)
-- > 25.4 +/- 0.6
--
-- For this reason, the convenience operators '(*~)' and '(~*)' have been
-- provided, where @n *~ x = sum (replicate n x)@, and '(~*)' is the
-- opposite.  The mneumonic is is that the @~@ is on the side of the
-- "uncertain" value.
--

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
--  * For 'RealFrac' and 'RealFloat' instances, most functions are
--  defined to be from the central value/most likely value.  However,
--  'ceiling' and 'floor' are taken to include the "entire range":
--
--  > > ceiling (7.8 +/- 0.4)
--  > 9             -- as if calling `ceiling 8.2`
--  > > floor (14.1 +/- 0.3)
--  > 12            -- as if calling `floor 13.8`
data Uncertain a = !a :+- !a deriving Generic

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
    signum (x :+- dx)       = signum x :+- dx
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

instance Eq a => Eq (Uncertain a) where
    (x :+- _) == (y :+- _) = x == y

instance Ord a => Ord (Uncertain a) where
    compare (x :+- _) (y :+- _) = compare x y

instance (Real a, Fractional a, Floating a) => Real (Uncertain a) where
    toRational (x :+- _) = toRational x

instance (Floating a, RealFrac a) => RealFrac (Uncertain a) where
    properFraction (x :+- dx) = let (j,k) = properFraction x in (j, k :+- dx)
    truncate                  = truncate . uVal
    round                     = round    . uVal
    ceiling                   = ceiling  . uncurry (+) . uComponents
    floor                     = floor    . uncurry (-) . uComponents

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

-- | Given a number and a precision, provides an 'Uncertain' with that
-- amount of precision, in decimal places.
--
-- > 6.194 `withPrecision` 2
-- 6.2 +/- 0.1
-- > 84723 `withPrecision` 3
-- 84700 +/- 100.
-- > 7.2 `withPrecision` 5
-- 7.2 +/- 1.e-4
withPrecision :: (RealFrac a, Floating a)
              => a      -- ^ number
              -> Int    -- ^ digits of precision/significant figures
              -> Uncertain a
withPrecision x n = x' :+- dx'
  where
    leading = negate . floor . logBase 10 $ x :: Integer
    uncert  = leading - 1 + fromIntegral n
    rounder = 10 ** fromIntegral uncert
    x'      = (/ rounder) . fromIntegral . round' . (* rounder) $ x
    dx'     = 1 / rounder

-- | Construct 'Uncertain's "by hand".
--
-- > > (7.18 +/- 0.03) * (2.3 +/- 0.7)
-- > 17.0 +/- 5.
--
-- Its main purpose is to make sure the uncertainties are reasonable and
-- positive.
(+/-) :: Num a
      => a      -- ^ central/most likely value
      -> a      -- ^ uncertainty/sigma/root variance
      -> Uncertain a
x +/- dx = x :+- abs dx

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

-- | The "sum of repeated sampling":
--
-- > n *~ x = sum (replicate n x)
--
-- See the @Gotchas@ section for more information.
--
-- Where @n * x@ assumes you sampled @x@ once and are multiplying it by
-- @n@, @n *~ x@ assumes you are sampling @x@ @n@ different times, with
-- each time independently varying.
--
-- There is a flipped version, '(~*)'; the mnemonic is that the @~@ is on
-- the side with the "uncertain" value.
--
-- '(~*)' is to '(*)' as '(^)' is to '(**)'.
(*~) :: Floating a => Int -> Uncertain a -> Uncertain a
n *~ x = sum (replicate n x)

-- | The flipped version of '(*~)'.  See documentation for '(*~)' and the
-- @Gotchas@ section for more information.
--
-- > x ~* n = sum (replicate n x)
--
-- The mnemonic is that the @~@ is on the side with the "uncertain" value.
--
-- '(~*)' is to '(*)' as '(^)' is to '(**)'.
(~*) :: Floating a => Uncertain a -> Int -> Uncertain a
(~*) = flip (*~)

-- $apply
--
-- 'uMap', 'uMap2', and 'uMap3' allow you to apply numerical functions on
-- one, two, or three variables (of the same type) to "uncertain" values.
-- This is only necessary when you have a non-generic/polymorphic function;
-- that is, a function on a specific type, like 'Double'.  It uses an
-- epsilon of 0.001 in order to approximate the necessary first partial
-- derivatives of the function.
--
-- > > let x = 1.52 +/- 0.07
-- > > let y = 781.4 +/- 0.3
-- > > let z = 1.53e-1 `withPrecision` 3
-- > > let f :: Double -> Double -> Double -> Double;
-- >           f a b c = b ** exp (c / (2 * a))
-- > > uMap3 f x y z
-- > 13.0 +/- 2.
--
-- For functions that vary very rapidly at the desired point, 0.001 might
-- not provide a precise enough window; see the variants 'uMap'', 'uMap2'',
-- and 'uMap3'', which allow the epsilon parameter to be adjusted.
--
-- If you want to apply a multivariable function and and only *some* of
-- your arguments are 'Uncertain', you can use 'certain' to lift an 'a' to
-- an 'Uncertain a'.
--
-- > > let y' = 2
-- > > x * 2
-- > 3.0 +/- 0.1
-- > > uMap2 (*) x (certain y)
-- > 3.0 +/- 0.1
--
-- Of course, if you have a literal, you can just take advantage of the
-- 'Num' and 'Fractional' instances:
--
-- > > uMap2 (*) x 2
-- > 3.0 +/- 0.1

-- | Applies a given numerical function to an 'Uncertain'.
--
-- > > let x = 5.1 +/- 0.2
-- > > sin x
-- > -0.93 +/- 8.e-2
-- > > uMap sin x
-- > -0.93 +/- 8.e-2
uMap :: (Fractional a, Real a, Floating b)
     => (a -> b)    -- ^ f
     -> Uncertain a -> Uncertain b
uMap = uMap' epsilon

-- | Applies a given numerical function on two variables (of the same type)
-- to two 'Uncertain's.
--
-- > > let x = 5.1 +/- 0.2
-- > > let y = 194 +/- 7
-- > > (\x y -> y / sin x) x y
-- > -210.0 +/- 20.
-- > > uMap2 (\x y -> y / sin x) x y
-- > -210 +/- 20.
uMap2 :: (Fractional a, Real a, Floating b)
      => (a -> a -> b)      -- ^ f
      -> Uncertain a -> Uncertain a -> Uncertain b
uMap2 = uMap2' epsilon

-- | Applies a given numerical function on three variables (of the same
-- type) to three 'Uncertain's.
--
-- > > let x = 5.1 +/- 0.2
-- > > let y = 194 +/- 7
-- > > let z = 0.832 +/- 0.006
-- > > (\x y z -> logBase z (abs (y / sin x))) x y z
-- > -29.0 +/- 1.
-- > > uMap3 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.0 +/- 1.
uMap3 :: (Fractional a, Real a, Floating b)
      => (a -> a -> a -> b)     -- ^ f
      -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3 = uMap3' epsilon


-- $applyeps
--
-- 'uMap'', 'uMap2'', and 'uMap3'' allow you to apply numerical functions
-- on one, two, or three variables (of the same type) to "uncertain"
-- values.  This is only necessary when you have a non-generic/polymorphic
-- function; that is, a function on a specific type, like 'Double'.  They
-- are like 'uMap', 'uMap2', and 'uMap3', except you can specify the
-- epsilon parameter used in approximating the first partial derivatives.
--
-- > > let x = 1.52 +/- 0.07
-- > > let y = 781.4 +/- 0.3
-- > > let z = 1.53e-1 `withPrecision` 3
-- > > let f :: Double -> Double -> Double -> Double;
-- >           f a b c = b ** exp (c / (2 * a))
-- > > uMap3 0.001 f x y z
-- > 13.0 +/- 2.
--
-- The more rapidly the function varies at the desired point, the smaller
-- @epsilon@ should be.
--
-- If you want to apply a multivariable function and and only *some* of
-- your arguments are 'Uncertain', you can use 'certain' to lift an 'a' to
-- an 'Uncertain a'.
--
-- > > let y' = 2
-- > > x * 2
-- > 3.0 +/- 0.1
-- > > uMap2 0.001 (*) x (certain y)
-- > 3.0 +/- 0.1
--
-- Of course, if you have a literal, you can just take advantage of the
-- 'Num' and 'Fractional' instances:
--
-- > > uMap2 0.001 (*) x 2
-- > 3.0 +/- 0.1


-- | Maps a given numerical function onto an 'Uncertain'.  Accepts an
-- @epsilon@ parameter which determines the desired accuracy of the first
-- derivative approximation used in the determination of the resulting
-- uncertainty.
--
-- > > let x = 5.1 +/- 0.2
-- > > sin x
-- > -0.93 +/- 8.e-2
-- > > uMap' 0.001 sin x
-- > -0.93 +/- 8.e-2
-- > > uMap' 0.5 (** 3) x
-- > -0.93 +/- 7.e-2
--
-- See 'uMap'.
uMap' :: (Fractional a, Real a, Floating b)
      => a          -- ^ epsilon
      -> (a -> b)   -- ^ f
      -> Uncertain a -> Uncertain b
uMap' eps f (x :+- dx) = f x :+- dx'
  where
    dx'   = abs (realToFrac dx * dfdx)
    dfdx  = (f (x + eps) - f (x - eps)) / (realToFrac eps * 2)

-- | Applies a given numerical function on two variables of the same type
-- to two 'Uncertain's.  Accepts an @epsilon@ parameter, which determines
-- the desired accuracy of the first partial derivative approximations used
-- in the determination of the resulting uncertainty.
--
-- > > let x = 5.1 +/- 0.2
-- > > let y = 194 +/- 7
-- > > (\x y -> y / sin x) x y
-- > -210.0 +/- 20.
-- > > uMap2' 0.001 (\x y -> y / sin x) x y
-- > -210 +/- 20.
-- > > uMap2' 0.75 (\x y -> y / sin x) x y
-- > -210 +/- 80.
--
-- See 'uMap2'.
uMap2' :: (Fractional a, Real a, Floating b)
       => a                 -- ^ epsilon
       -> (a -> a -> b)     -- ^ f
       -> Uncertain a -> Uncertain a -> Uncertain b
uMap2' eps f (x :+- dx) (y :+- dy) = f x y :+- dxy
  where
    dxy  = sqrt ((realToFrac dx * dfdx)^2 + (realToFrac dy * dfdy)^2)
    dfdx = (f (x + eps) y - f (x - eps) y) / (realToFrac eps * 2)
    dfdy = (f x (y + eps) - f x (y - eps)) / (realToFrac eps * 2)

-- | Applies a given numerical function on three variables of the same type
-- to two 'Uncertain's.  Accepts an @epsilon@ parameter, which determines
-- the desired accuracy of the first partial derivative approximations used
-- in the determination of the resulting uncertainty.
--
-- > > let x = 5.1 +/- 0.2
-- > > let y = 194 +/- 7
-- > > let z = 0.832 +/- 0.006
-- > > (\x y z -> logBase z (abs (y / sin x))) x y z
-- > -29.0 +/- 1.
-- > > uMap3' 0.001 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.0 +/- 1.
-- > > uMap3' 0.5 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.1 +/- 0.5
--
-- See 'uMap3'.
uMap3' :: (Fractional a, Real a, Floating b)
       => a                     -- ^ epsilon
       -> (a -> a -> a -> b)    -- ^ f
       -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3' eps f (x :+- dx) (y :+- dy) (z :+- dz) = f x y z :+- dxyz
  where
    dxyz = sqrt ((realToFrac dx * dfdx)^2
               + (realToFrac dy * dfdy)^2
               + (realToFrac dz * dfdz)^2)
    dfdx = (f (x + eps) y z - f (x - eps) y z) / (realToFrac eps * 2)
    dfdy = (f x (y + eps) z - f x (y - eps) z) / (realToFrac eps * 2)
    dfdz = (f x y (z + eps) - f x y (z - eps)) / (realToFrac eps * 2)

-- Utilities

epsilon :: Fractional a => a
epsilon = 0.001

round' :: RealFrac a => a -> Integer
round' = round

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

