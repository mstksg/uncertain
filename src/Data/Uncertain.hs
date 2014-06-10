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
-- > ghci> let x = 1.52 +/- 0.07
-- > ghci> let y = 781.4 +/- 0.3
-- > ghci> let z = 1.53e-1 `withPrecision` 3
-- > ghci> cosh x
-- > 2.4 +/- 0.2
-- > ghci> exp x / z * sin (y ** z)
-- > 10.8 +/- 0.9
-- > ghci> pi + 3 * logBase x y
-- > 50.87 +/- 2.e-2
--
-- If you only have "normal" functions, like `Double -> Double -> Double`,
-- you can use 'uMap', 'uMap2', and 'uMap3'.
module Data.Uncertain (
    Uncertain
  , certain
  , withPrecision
  , (+/-)
  , uVal
  , uVar
  , uNormalize
  , uComponents
  , uMap'
  , uMap
  , uMap2'
  , uMap2
  , uMap3'
  , uMap3
  , main
  ) where

import Control.Arrow
import Prelude hiding    ((^))
import qualified Prelude ((^))

-- | A data type representing a number with an associated
-- experimental/measurement uncertainty.
--
-- So that you can treat it like a normal number, it is an instance of
-- 'Num', 'Fractional', 'Floating', and 'Real', 'RealFrac', and
-- 'RealFloat'.
--
-- You can apply any function or combination/composition of functions from
-- any of the above typeclasses:
--
-- > ghci> let x = 12.4 +/- 0.3
-- > ghci> sqrt x
-- > 3.52 +/- 4.e-2
-- > ghci> let y = 7.18 `sigFig` 3
-- > ghci> y
-- > 7.18 +/- 1.e-2
-- > ghci> log (sqrt x * cos y) ** y
-- > 0.18 +/- 3.e-2
-- > ghci> let z = certain 15       -- or let z = 15
-- > ghci> log (sqrt x * cos y) ** y * z
-- > 2.7 +/- 0.4
-- > ghci> let z' = 15 +/- 4
-- > ghci> log (sqrt x * cos y) ** y * z'
-- > 2.7 +/- 0.8
--
-- All typeclasses are fully defined, so go crazy!  You should technically
-- be able to do anything you can possibly do with a generic instance of
-- 'Num', 'Floating', etc.
--
-- The 'Show' instance displays the "normalized" value (see 'uNormalize').
data Uncertain a = !a :+- !a

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
-- > ghci> certain 5 * certin 9
-- > certain 45.0
-- > ghci> (18.2 +/- 0.3) ** certain (1/2)
-- > 4.271 +/- 4.e-3
--
-- Usually, this can be ignored and only the actual literal needs be
-- entered.
--
-- > ghci> 5 * 9 :: Uncertain Double
-- > certain 45.0
-- > ghci> (18.2 +/- 0.3) ** (1/2)
-- > 4.271 +/- 4.e-3
--
-- Use this for when what you are wrapping is not a literal.
--
-- > ghci> map certain [1,2,3]
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
withPrecision :: (RealFrac a, Floating a) => a -> Int -> Uncertain a
withPrecision x n = x' :+- dx'
  where
    leading = negate . floor . logBase 10 $ x :: Integer
    uncert  = leading - 1 + fromIntegral n
    rounder = 10 ** fromIntegral uncert
    x'      = (/ rounder) . fromIntegral . round' . (* rounder) $ x
    dx'     = 1 / rounder

-- | Construct 'Uncertain's "by hand".
--
-- > ghci> (7.18 +/- 0.03) * (2.3 +/- 0.7)
-- > 17.0 +/- 5.
--
-- Its main purpose is to make sure the uncertainties are reasonable and
-- positive.
(+/-) :: Num a => a -> a -> Uncertain a
x +/- dx = x :+- abs dx

-- | Get the (central/most likely) value out of an 'Uncertain'.
--
-- > ghci> uVal (15.2 +/- 0.4)
-- > 15.2
uVal :: Uncertain a -> a
uVal (x :+- _) = x

-- | Get the uncertainty/squared variance out of an 'Uncertain'.
--
-- > ghci> uVar (15.2 +/- 0.4)
-- > 0.4
uVar :: Uncertain a -> a
uVar (_ :+- dx) = dx

-- | "Normalize" an 'Uncertain'.
--
-- This means that the value can't have any more precision than implied by
-- the uncertainty, and that the uncertainty can't have more than one digit
-- of precision/significance.
--
-- > ghci> uComponents $ uNormalize (15.235812734 +/- 0.0348513)
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

-- | Returns the two components (the central value and the squared
-- variance) of an 'Uncertain'.
--
-- > uComponents = uVal &&& uVar
uComponents :: Uncertain a -> (a, a)
uComponents = uVal &&& uVar

-- | Maps a given numerical function onto an 'Uncertain'.  Accepts
-- an @epsilon@ parameter which determines the desired accuracy of the
-- first derivative approximation used in the determination of the
-- resulting uncertainty.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> sin x
-- > -0.93 +/- 8.e-2
-- > ghci> uMap' 0.001 sin x
-- > -0.93 +/- 8.e-2
-- > ghci> uMap' 0.5 (** 3) x
-- > -0.93 +/- 7.e-2
--
-- See 'uMap'.
uMap' :: (Fractional a, Real a, Floating b)
      => a
      -> (a -> b)
      -> Uncertain a -> Uncertain b
uMap' eps f (x :+- dx) = f x :+- dx'
  where
    dx'   = abs (realToFrac dx * dfdx)
    dfdx  = (f (x + eps) - f (x - eps)) / (realToFrac eps * 2)

-- | Applies a given numerical function to an 'Uncertain'.  To approximate
-- a first derivative, an epsilon of 0.001 is used.  To adjust this (if
-- the result of your function varies very rapidly around the point in
-- question), use 'uMap''.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> sin x
-- > -0.93 +/- 8.e-2
-- > ghci> uMap sin x
-- > -0.93 +/- 8.e-2
uMap :: (Fractional a, Real a, Floating b)
     => (a -> b)
     -> Uncertain a -> Uncertain b
uMap = uMap' epsilon

-- | Applies a given numerical function on two variables of the same type
-- to two 'Uncertain's.  Accepts an @epsilon@ parameter, which determines
-- the desired accuracy of the first partial derivative approximations used
-- in the determination of the resulting uncertainty.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> let y = 194 +/- 7
-- > ghci> (\x y -> y / sin x) x y
-- > -210.0 +/- 20.
-- > ghci> uMap2' 0.001 (\x y -> y / sin x) x y
-- > -210 +/- 20.
-- > ghci> uMap2' 0.75 (\x y -> y / sin x) x y
-- > -210 +/- 80.
--
-- If one of the arguments is not an 'Uncertain' but a normal number, you
-- can use 'certain' to create an 'Uncertain' with zero uncertainty.
--
-- > ghci> let y' = 2
-- > ghci> x * 2
-- > 10.2 +/- 0.4
-- > ghci> uMap2' 0.001 (*) x (certain y')
-- > 10.2 +/- 0.4
--
-- (Of course, if you are using a literal, you can just use the 'Num'
-- instance for 'Uncertain':)
--
-- > ghci> uMap2' 0.001 (*) x 2
-- > 10.2 +/- 0.4
--
-- See 'uMap2'.
uMap2' :: (Fractional a, Real a, Floating b)
       => a
       -> (a -> a -> b)
       -> Uncertain a -> Uncertain a -> Uncertain b
uMap2' eps f (x :+- dx) (y :+- dy) = f x y :+- dxy
  where
    dxy  = sqrt ((realToFrac dx * dfdx)^2 + (realToFrac dy * dfdy)^2)
    dfdx = (f (x + eps) y - f (x - eps) y) / (realToFrac eps * 2)
    dfdy = (f x (y + eps) - f x (y - eps)) / (realToFrac eps * 2)

-- | Applies a given numerical function on two variables (of the same type)
-- to two 'Uncertain's.  To approximate the first partial derivatives, an
-- epsilon of 0.001 is used.  To adjust this (if the result of your
-- function varies very rapidly around the points in question), use
-- 'uMap2''.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> let y = 194 +/- 7
-- > ghci> (\x y -> y / sin x) x y
-- > -210.0 +/- 20.
-- > ghci> uMap2 (\x y -> y / sin x) x y
-- > -210 +/- 20.
--
-- If one of the arguments is not an 'Uncertain' but a normal number, you
-- can use 'certain' to create an 'Uncertain' with zero uncertainty.
--
-- > ghci> let y' = 2
-- > ghci> x * 2
-- > 10.2 +/- 0.4
-- > ghci> uMap2 (*) x (certain y)
-- > 10.2 +/- 0.4
--
-- (Of course, if you are using a literal, you can just use the 'Num'
-- instance for 'Uncertain':)
--
-- > ghci> uMap2 (*) x 2
-- > 10.2 +/- 0.4
uMap2 :: (Fractional a, Real a, Floating b)
      => (a -> a -> b)
      -> Uncertain a -> Uncertain a -> Uncertain b
uMap2 = uMap2' epsilon

-- | Applies a given numerical function on three variables of the same type
-- to two 'Uncertain's.  Accepts an @epsilon@ parameter, which determines
-- the desired accuracy of the first partial derivative approximations used
-- in the determination of the resulting uncertainty.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> let y = 194 +/- 7
-- > ghci> let z = 0.832 +/- 0.006
-- > ghci> (\x y z -> logBase z (abs (y / sin x))) x y z
-- > -29.0 +/- 1.
-- > ghci> uMap3' 0.001 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.0 +/- 1.
-- > ghci> uMap3' 0.5 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.1 +/- 0.5
--
-- If one of the arguments is not an 'Uncertain' but a normal number, you
-- can use 'certain' to create an 'Uncertain' with zero uncertainty.
--
-- > ghci> let z' = 9
-- > ghci> x * y + 9
-- > 1000.0 +/- 50.
-- > ghci> uMap3' 0.001 (\x y z -> x * y + z) x y (certain z')
-- > 1000.0 +/- 50.
--
-- (Of course, if you are using a literal, you can just use the 'Num'
-- instance for 'Uncertain':)
--
-- > ghci> uMap3' 0.001 (\x y z -> x * y + z) x y 9
-- > 1000.0 +/- 50.
--
-- See 'uMap3'.
uMap3' :: (Fractional a, Real a, Floating b)
       => a
       -> (a -> a -> a -> b)
       -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3' eps f (x :+- dx) (y :+- dy) (z :+- dz) = f x y z :+- dxyz
  where
    dxyz = sqrt ((realToFrac dx * dfdx)^2
               + (realToFrac dy * dfdy)^2
               + (realToFrac dz * dfdz)^2)
    dfdx = (f (x + eps) y z - f (x - eps) y z) / (realToFrac eps * 2)
    dfdy = (f x (y + eps) z - f x (y - eps) z) / (realToFrac eps * 2)
    dfdz = (f x y (z + eps) - f x y (z - eps)) / (realToFrac eps * 2)

-- | Applies a given numerical function on three variables (of the same type)
-- to two 'Uncertain's.  To approximate the first partial derivatives, an
-- epsilon of 0.001 is used.  To adjust this (if the result of your
-- function varies very rapidly around the points in question), use
-- 'uMap3''.
--
-- > ghci> let x = 5.1 +/- 0.2
-- > ghci> let y = 194 +/- 7
-- > ghci> let z = 0.832 +/- 0.006
-- > ghci> (\x y z -> logBase z (abs (y / sin x))) x y z
-- > -29.0 +/- 1.
-- > ghci> uMap3 (\x y z -> logBase z (y / sin x)) x y z
-- > -29.0 +/- 1.
--
-- If one of the arguments is not an 'Uncertain' but a normal number, you
-- can use 'certain' to create an 'Uncertain' with zero uncertainty.
--
-- > ghci> let z' = 9
-- > ghci> x * y + 9
-- > 1000.0 +/- 50.
-- > ghci> uMap3 (\x y z -> x * y + z) x y (certain z')
-- > 1000.0 +/- 50.
--
-- (Of course, if you are using a literal, you can just use the 'Num'
-- instance for 'Uncertain':)
--
-- > ghci> uMap3 (\x y z -> x * y + z) x y 9
-- > 1000.0 +/- 50.
uMap3 :: (Fractional a, Real a, Floating b)
      => (a -> a -> a -> b)
      -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3 = uMap3' epsilon


main :: IO ()
main = do
    let x = 1.52  +/- 0.07  :: Uncertain Double
        y = 781.4 +/- 0.2   :: Uncertain Double
        z = 0.153 +/- 0.004 :: Uncertain Double
    putStrLn $ "x = " ++ show x
    putStrLn $ "y = " ++ show y
    putStrLn $ "z = " ++ show z
    let e = pi + exp x + sqrt (sin y ** 2 + log x * atan (x**y) / exp (2 * z))
    putStrLn   "pi + exp x + sqrt ((sin y)**2 + log z * atan (x**y) / exp (2 * z))"
    putStrLn $ "  = " ++ show e


epsilon :: Fractional a => a
epsilon = 0.001

round' :: RealFrac a => a -> Integer
round' = round

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

