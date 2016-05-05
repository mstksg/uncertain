{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE ViewPatterns        #-}
#endif

-- |
-- Module      : Data.Uncertain
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--

module Data.Uncertain
  ( -- * 'Uncert'
    Uncert
#if __GLASGOW_HASKELL__ >= 708
  , pattern (:+/-)
#endif
    -- ** Creating 'Uncert' values
  , (+/-), exact, withPrecision, withPrecisionAtBase, withVar, fromSamples
    -- ** Inspecting properties
  , uMean, uVar, uStd, uMeanVar, uMeanStd, uRange
    -- * Applying arbitrary functions
  , liftU
  , liftU2, liftU3, liftU4, liftU5, liftUF
    -- * Utility functions
  , uNormalize, uNormalizeAtBase
  , uShow, uShowsPrec
  )
  where

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
-- propagate appropriately.  You can also lift arbitrary (sufficiently
-- polymorphic) functions with 'liftU', 'liftUF', 'liftU2' and family.
--
-- @
-- ghci> let x = 1.52 '+/-' 0.07
-- ghci> let y = 781.4 +/- 0.3
-- ghci> let z = 1.53e-1 `'withPrecision'` 3
-- ghci> cosh x
-- 2.4 +/- 0.2
-- ghci> exp x / z * sin (y ** z)
-- 10.9 +/- 0.9
-- ghci> pi + 3 * logBase x y
-- 52 +/- 5
-- @
--
-- Uncertaintly is properly propagated according to the second-degree
-- taylor series approximations of the applied functions.  However, if the
-- higher-degree terms are large with respect to to the means and
-- variances of the uncertain values, these approximations may be
-- inaccurate.
--
-- Can be created with 'exact' to represent an "exact" measurement with no
-- uncertainty, '+/-' and ':+/-' to specify a standard deviation as
-- a range, 'withPrecision' to specify through decimal precision, and
-- 'withVar' to specify with a variance.  Can also be inferred from a list
-- of samples with 'fromSamples'
--
-- @
-- 7.13 '+/-' 0.05
-- 91800 +/- 100
-- 12.5 `'withVar'` 0.36
-- 'exact' 7.9512
-- 81.42 `'withPrecision'` 4
-- 7    :: Uncertain Double
-- 9.18 :: Uncertain Double
-- 'fromSamples' [12.5, 12.7, 12.6, 12.6, 12.5]
-- @
--
-- Can be deconstructed with ':+/-', the pattern synonym/pseudo-constructor
-- which matches on the mean and a standard deviation.  You can also access
-- properties with 'uMean', 'uStd', 'uVar', 'uMeanStd', 'uMeanVar',
-- 'uRange', etc.
--
-- It's important to remember that each "occurrence" represents a unique
-- independent sample, so:
--
-- @
-- ghci> let x = 15 '+/-' 2 in x + x
-- 30 +/- 3
--
-- ghci> let x = 15 +/- 2 in x*2
-- 30 +/- 4
-- @
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

-- | Get the mean/central value/expected value of an 'Uncert'.
uMean :: Uncert a -> a
uMean = _uMean

-- | Get the /variance/ of the uncertainty of an 'Uncert', proportional to
-- the square of "how uncertain" a value is.  Is the square of 'uStd'.
uVar :: Uncert a -> a
uVar = _uVar

-- | Get the /standard deviation/ of the uncertainty of an 'Uncert',
-- proportional to "how uncertain" a value is.
--
-- Very informally, it can be thought of as the interval above and below
-- the mean that about 68% of sampled values will fall under after repeated
-- sampling, or as the range that one is 68% sure the true value is within.
--
-- Is the square root of 'uVar'.
uStd :: Floating a => Uncert a -> a
uStd = sqrt . uVar

-- | Create an 'Uncert' with an exact value and 0 uncertainty.
exact
    :: Num a
    => a            -- ^ The exact value
    -> Uncert a
exact x = Un x 0

infixl 6 +/-
infixl 6 :+/-

-- | Create an 'Uncert' around a central value and a given "range" of
-- uncertainty.  The range is interpreted as the standard deviation of the
-- underlying random variable.  Might be preferrable over ':+/-' because it
-- is more general (doesn't require a 'Floating' constraint) and looks
-- a bit nicer.
--
-- See 'uStd' for more details.
(+/-)
    :: Num a
    => a            -- ^ The mean or central value
    -> a            -- ^ The standard deviation of the underlying uncertainty
    -> Uncert a
x +/- dx = Un x (dx*dx)

-- | Create an 'Uncert' around a central value, specifying its uncertainty
-- with a given /variance/.  The variance is taken to be proportional to
-- the square of the range of uncertainty.  See 'uStd' for more details.
--
-- "Negative variances" are treated as positive.
withVar
    :: Num a
    => a            -- ^ The mean or central value
    -> a            -- ^ The variance of the underlying uncertainty
    -> Uncert a
withVar x vx = Un x (abs vx)

#if __GLASGOW_HASKELL__ >= 708
-- | Pattern match on an 'Uncert' with its central value and its standard
-- deviation (see 'uStd' for clarification).
--
-- Can also be used to /construct/ an 'Uncert', identically as '+/-'.
--
-- /Note:/ Only supported on GHC 7.8 and above.
--
pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)
#endif

-- | Infer an 'Uncert' from a given list of independent /samples/ of an
-- underlying uncertain or random distribution.
fromSamples :: Fractional a => [a] -> Uncert a
fromSamples = makeUn . foldStats
  where
    makeUn (H3 x0 x1 x2) = Un μ v
      where
        μ = x1/x0
        v = x2/x0 - μ*μ     -- maybe use pop var?
    foldStats = flip foldl' (H3 0 0 0) $
                  \(H3 s0 s1 s2) x ->
                    H3 (s0 + 1) (s1 + x) (s2 + x*x)

-- | Retrieve both the mean (central) value and the underlying variance of
-- an 'Uncert' together.
--
-- @uMeanVar ≡ 'uMean' &&& 'uVar'@
uMeanVar :: Uncert a -> (a, a)
uMeanVar (Un x vx) = (x, vx)

-- | Retreve both the mean (central) value and the underlying standard
-- deviation of an 'Uncert' together.  (See 'uStd' for more details)
--
-- @uMeanStd ≡ 'uMean' &&& 'uStd'@
uMeanStd :: Floating a => Uncert a -> (a, a)
uMeanStd (Un x vx) = (x, sqrt vx)

-- | Retrieve the "range" of the underlying distribution of an 'Uncert',
-- derived from the standard deviation, where which approximly 68% of
-- sampled values are expected to occur (or within which you are 68%
-- certain the true value is).
--
-- @uRange (x +/- dx) ≡ (x - dx, x + dx)@
uRange :: Floating a => Uncert a -> (a, a)
uRange u = let x :+/- dx = u
           in  (x - dx, x + dx)

-- | Like 'withPrecision', except takes a number of "digits" of precision in
-- the desired numeric base.  For example, in base 2, takes the number of
-- /bits/ of precision.
--
-- @'withPrecision' ≡ withPrecisionAtBase 10@
withPrecisionAtBase
    :: (Floating a, RealFrac a)
    => Int          -- ^ The base to determine precision with respect to
    -> a            -- ^ The approximate value of the 'Uncert'
    -> Int          -- ^ The number of "digits" of precision to take
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

-- | Create an 'Uncert' about a given approximate central value, with the
-- given number of /digits of precision/ (in decimal notation).
--
-- @5.21 `withPrecision` 3 ≡ 5.21 '+/-' 0.01@
withPrecision
    :: (Floating a, RealFrac a)
    => a            -- ^ The approximate value of the 'Uncert'
    -> Int          -- ^ The number of "digits" of precision to take
    -> Uncert a
withPrecision = withPrecisionAtBase 10

-- | Like 'uNormalize', but takes a numerical base to round with respect
-- to.
--
-- @'uNormalize' ≡ uNormalizeAtBase 10@
uNormalizeAtBase
    :: (Floating a, RealFrac a)
    => Int          -- ^ The base to normalize with respect to
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

-- | Attempts to "normalize" an 'Uncert'.  Rounds the uncertainty (the
-- standard deviation) to one digit of precision, and rounds the central
-- moment up to the implied precision.
--
-- For example, it makes no real sense to have @542.185433 +/- 83.584@,
-- because the extra digits of @542.185433@ past the tens place has no
-- meaning because of the overpowering uncertainty.   Normalizing this
-- results in @540 +/- 80@.
--
-- Note that the 'Show' instance for 'Uncert' normalizes values before
-- showing them.
uNormalize
    :: (Floating a, RealFrac a)
    => Uncert a
    -> Uncert a
uNormalize = uNormalizeAtBase 10

instance (Show a, Floating a, RealFrac a) => Show (Uncert a) where
    showsPrec d = uShowsPrec d . uNormalize

-- | Like 'showsPrec' for 'Uncert', but does not normalize the value (see
-- 'uNormalize') before showing.  See documentation for 'showsPrec' for
-- more information on how this is meant to be used.
uShowsPrec :: (Show a, Floating a) => Int -> Uncert a -> ShowS
uShowsPrec d u = showParen (d > 5) $
                     showsPrec 6 x
                   . showString " +/- "
                   . showsPrec 6 dx
  where
    x :+/- dx = u

-- | Like 'show' for 'Uncert', but does not normalize the value (see
-- 'uNormalize') before showing.
--
-- @'show' ≡ uShow . 'uNormalize'@
uShow :: (Show a, Floating a) => Uncert a -> String
uShow u = uShowsPrec 0 u ""

-- | Lifts a multivariate numeric function on a container (given as an @f
-- a -> a@) to work on a container of 'Uncert's.  Correctly propagates the
-- uncertainty according to the second-order (multivariate) taylor
-- expansion of the function.  Note that if the higher-degree taylor series
-- terms are large with respect to the means and variances, this
-- approximation may be inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like '*', 'sqrt', 'atan2', etc.
--
-- @
-- ghci> liftUF (\[x,y,z] -> x*y+z) [12.2 +/- 0.5, 56 +/- 2, 0.12 +/- 0.08]
-- 680 +/- 40
-- @
--
liftUF
    :: (Traversable f, Fractional a)
    => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a))   -- ^ Function on container of values to lift
    -> f (Uncert a)         -- ^ Container of 'Uncert's to apply the function to
    -> Uncert a
liftUF f us = Un y vy
  where
    xs          = uMean <$> us
    vxs         = uVar  <$> us
    vxsL        = toList vxs
    (fx, dfxsh) = hessian' f xs
    dfxs        = fst <$> dfxsh
    hess        = snd <$> dfxsh
    y           = fx + hessQuad / 2
      where
        hessQuad = dot vxsL
                 . diag
                 . toList
                 $ fmap toList hess
    vy          = dot vxsL ((^ (2::Int)) <$> dfxs)
    dot x = sum . zipWith (*) x . toList
    diag = \case []        -> []
                 []   :yss -> diag (drop1 <$> yss)
                 (x:_):yss -> x : diag (drop1 <$> yss)
      where
        drop1 []     = []
        drop1 (_:zs) = zs

-- | Lifts a numeric function over an 'Uncert'.  Correctly propagates the
-- uncertainty according to the second-order taylor expansion expansion of
-- the function.  Note that if the higher-degree taylor series terms are
-- large with respect to the mean and variance, this approximation may be
-- inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like 'sqrt', 'sin', 'negate', etc.
--
-- @
-- ghci> liftU (\x -> log x ^ 2) (12.2 +/- 0.5)
-- 6.3 +/- 0.2
-- @
liftU
    :: Fractional a
    => (forall s. AD s (T.Tower a) -> AD s (T.Tower a))     -- ^ Function on values to lift
    -> Uncert a     -- ^ 'Uncert' to apply the function to
    -> Uncert a
liftU f (Un x vx) = Un y vy
  where
    fx:dfx:ddfx:_ = T.diffs0 f x
    y             = fx + ddfx * vx / 2
    vy            = dfx*dfx * vx

-- | Lifts a two-argument (curried) function over two 'Uncert's.  Correctly
-- propagates the uncertainty according to the second-order (multivariate)
-- taylor expansion expansion of the function.  Note that if the
-- higher-degree taylor series terms are large with respect to the mean and
-- variance, this approximation may be inaccurate.
--
-- Should take any function sufficiently polymorphic over numeric types, so
-- you can use things like '*', 'atan2', '**', etc.
--
-- @
-- ghci> liftU2 (\x y -> x**y) (13.5 +/- 0.1) (1.64 +/- 0.08)
-- 70 +/- 10
-- @
liftU2
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU2 f = curryH2 $ liftUF (uncurryH2 f)

-- | Lifts a three-argument (curried) function over three 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU3
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU3 f = curryH3 $ liftUF (uncurryH3 f)

-- | Lifts a four-argument (curried) function over four 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU4
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU4 f = curryH4 $ liftUF (uncurryH4 f)

-- | Lifts a five-argument (curried) function over five 'Uncert's.  See
-- 'liftU2' and 'liftUF' for more details.
liftU5
    :: Fractional a
    => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
    -> Uncert a
liftU5 f = curryH5 $ liftUF (uncurryH5 f)

instance Fractional a => Num (Uncert a) where
    (+)         = liftU2 (+)
    (*)         = liftU2 (*)
    (-)         = liftU2 (-)
    negate      = liftU negate
    abs         = liftU abs
    signum      = liftU signum
    fromInteger = exact . fromInteger

instance Fractional a => Fractional (Uncert a) where
    recip        = liftU recip
    (/)          = liftU2 (/)
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
    floatRadix      = floatRadix     . uMean
    floatDigits     = floatDigits    . uMean
    floatRange      = floatRange     . uMean
    decodeFloat     = decodeFloat    . uMean
    exponent        = exponent       . uMean
    isNaN           = isNaN          . uMean
    isInfinite      = isInfinite     . uMean
    isDenormalized  = isDenormalized . uMean
    isNegativeZero  = isNegativeZero . uMean
    isIEEE          = isIEEE         . uMean
    encodeFloat a b = exact (encodeFloat a b)
    significand     = liftU significand
    atan2           = liftU2 atan2

