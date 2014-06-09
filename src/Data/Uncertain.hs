module Data.Uncertain (
    Uncertain
  , certain
  , sigFigs
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

-- | Wraps a 'Num' in an 'Uncertain', associating it with an uncertainty of
-- @0@.  Represents a number you know exactly, with no uncertainty.
--
-- This is also the default 'fromInteger' method from the 'Num' instance.
--
-- > certain 5 * certin 9
-- certain 45.0
-- > (18.2 +/- 0.3) ** certain (1/2)
-- > 4.271 +/- 4.e-3
--
-- Usually, this can be ignored and only the actual literal needs be
-- entered.  Use this for explicit conversions.
-- 
-- > 5 * 9 :: Uncertain Double
-- certain 45.0
-- > (18.2 +/- 0.3) ** (1/2)
-- > 4.271 +/- 4.e-3
certain :: Num a => a -> Uncertain a
certain = (:+- 0)

sigFigs :: (RealFrac a, Floating a) => a -> Int -> Uncertain a
-- sigFig _ 0 = error "Must have positive significant figures"
sigFigs x n = x' :+- dx'
  where
    leading = negate . floor . logBase 10 $ x :: Integer
    uncert  = leading - 1 + fromIntegral n
    rounder = 10 ** fromIntegral uncert
    x'      = (/ rounder) . fromIntegral . round' . (* rounder) $ x
    dx'     = 1 / rounder

(+/-) :: Num a => a -> a -> Uncertain a
x +/- dx = x :+- abs dx

uVal :: Uncertain a -> a
uVal (x :+- _) = x

uVar :: Uncertain a -> a
uVar (_ :+- dx) = dx

round' :: RealFrac a => a -> Integer
round' = round

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

uNormalize :: (RealFrac a, Floating a) => Uncertain a -> Uncertain a
uNormalize (x :+- dx) = x' :+- dx'
  where
    uncert  = negate . floor . logBase 10 $ dx :: Integer
    rounder = 10 ** fromIntegral uncert
    roundTo = (/ rounder) . fromIntegral . round' . (* rounder)
    x'      = roundTo x
    dx'     = roundTo dx

uComponents :: Uncertain a -> (a, a)
uComponents = uVal &&& uVar

uMap' :: (Fractional a, Real a, Floating b)
      => a
      -> (a -> b)
      -> Uncertain a -> Uncertain b
uMap' delta f (x :+- dx) = f x :+- dx'
  where
    dx'   = abs (realToFrac dx * dfdx)
    dfdx  = (f (x + delta) - f (x - delta)) / (realToFrac delta * 2)

uMap :: (Fractional a, Real a, Floating b)
     => (a -> b)
     -> Uncertain a -> Uncertain b
uMap = uMap' epsilon

uMap2' :: (Fractional a, Real a, Floating b)
       => a
       -> (a -> a -> b)
       -> Uncertain a -> Uncertain a -> Uncertain b
uMap2' delta f (x :+- dx) (y :+- dy) = f x y :+- dxy
  where
    dxy  = sqrt ((realToFrac dx * dfdx)^2 + (realToFrac dy * dfdy)^2)
    dfdx = (f (x + delta) y - f (x - delta) y) / (realToFrac delta * 2)
    dfdy = (f x (y + delta) - f x (y - delta)) / (realToFrac delta * 2)

uMap2 :: (Fractional a, Real a, Floating b)
      => (a -> a -> b)
      -> Uncertain a -> Uncertain a -> Uncertain b
uMap2 = uMap2' epsilon

uMap3' :: (Fractional a, Real a, Floating b)
       => a
       -> (a -> a -> a -> b)
       -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3' delta f (x :+- dx) (y :+- dy) (z :+- dz) = f x y z :+- dxyz
  where
    dxyz = sqrt ((realToFrac dx * dfdx)^2
               + (realToFrac dy * dfdy)^2
               + (realToFrac dz * dfdz)^2)
    dfdx = (f (x + delta) y z - f (x - delta) y z) / (realToFrac delta * 2)
    dfdy = (f x (y + delta) z - f x (y - delta) z) / (realToFrac delta * 2)
    dfdz = (f x y (z + delta) - f x y (z - delta)) / (realToFrac delta * 2)

uMap3 :: (Fractional a, Real a, Floating b)
      => (a -> a -> a -> b)
      -> Uncertain a -> Uncertain a -> Uncertain a -> Uncertain b
uMap3 = uMap3' epsilon

epsilon :: Fractional a => a
epsilon = 0.001

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
    pi = certain pi
    exp  (x :+- dx) = exp x  :+- (exp x * dx)
    sqrt (x :+- dx) = sqrt x :+- (dx/(2 * sqrt x))
    log  (x :+- dx) = log x  :+- (dx/x)
    (x :+- dx) ** (y :+- dy) = (x ** y) :+- sqrt ((y * x ** (y-1) * dx)^2 + (x ** y * log x * dy)^2)
    sin  (x :+- dx) = sin x  :+- (abs (cos x) * dx)
    cos  (x :+- dx) = cos x  :+- (abs (sin x) * dx)
    asin (x :+- dx) = asin x :+- (dx / sqrt (1 - x^2))
    atan (x :+- dx) = atan x :+- (dx / (x^2 + 1))
    acos (x :+- dx) = acos x :+- (dx / sqrt (1 - x^2))
    sinh (x :+- dx) = sinh x :+- (cosh x * dx)
    tanh (x :+- dx) = tanh x :+- (dx / cosh x ^ 2)
    cosh (x :+- dx) = cosh x :+- (sinh x * dx)
    asinh (x :+- dx) = asinh x :+- (dx / sqrt (x^2 + 1))
    atanh (x :+- dx) = atanh x :+- (dx / abs (1 - x^2))
    acosh (x :+- dx) = acosh x :+- (dx / sqrt (x^2 - 1))

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

