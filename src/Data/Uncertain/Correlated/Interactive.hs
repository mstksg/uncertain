{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Uncertain.Correlated.Interactive
  ( CVar
  , sampleUncert
  , sampleExact
  , resolveUncert
  , liftCF
  , constC, liftC, liftC2, liftC3, liftC4, liftC5
  )
  where

import           Control.Monad.Trans.State
import           Data.Hople
import           Data.IORef
import           Data.Tuple
import           Data.Uncertain
import           System.IO.Unsafe                   (unsafePerformIO)
import qualified Data.IntMap.Strict                 as M
import qualified Data.Uncertain.Correlated          as C
import qualified Data.Uncertain.Correlated.Internal as C


newtype CVar = CV { unwrapCV :: forall s. C.CVar s Double
                  }

globalCorrMap :: IORef (M.Key, M.IntMap (Uncert Double))
{-# NOINLINE globalCorrMap #-}
globalCorrMap = unsafePerformIO $ newIORef (0, M.empty)

runCorrIO :: (forall s. C.Corr s Double a) -> IO a
runCorrIO c = do
    atomicModifyIORef' globalCorrMap (swap . runState (C.corrToState c))

sampleUncert :: Uncert Double -> IO CVar
sampleUncert u = runCorrIO $ wrapCV (C.sampleUncert u)

sampleExact :: Double -> IO CVar
sampleExact d = runCorrIO $ wrapCV (C.sampleExact d)

resolveUncert :: CVar -> IO (Uncert Double)
resolveUncert = \case CV v -> runCorrIO $ C.resolveUncert v

wrapCV :: (forall t. C.Corr t Double (C.CVar t Double)) -> C.Corr s Double CVar
wrapCV = fmap (\v -> CV (C.dephantom v))

liftCF
    :: Functor f
    => (forall s. f (C.CVar s Double) -> C.CVar s Double)
    -> f CVar
    -> CVar
liftCF f cs = CV $ f (unwrapCV <$> cs)

constC :: Double -> CVar
constC x = CV (C.constC x)

liftC
    :: (forall s. C.CVar s Double -> C.CVar s Double)
    -> CVar
    -> CVar
liftC f = curryH1 $ liftCF (uncurryH1 f)

liftC2
    :: (forall s. C.CVar s Double -> C.CVar s Double -> C.CVar s Double)
    -> CVar
    -> CVar
    -> CVar
liftC2 f = curryH2 $ liftCF (uncurryH2 f)

liftC3
    :: (forall s. C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double)
    -> CVar
    -> CVar
    -> CVar
    -> CVar
liftC3 f = curryH3 $ liftCF (uncurryH3 f)

liftC4
    :: (forall s. C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double)
    -> CVar
    -> CVar
    -> CVar
    -> CVar
    -> CVar
liftC4 f = curryH4 $ liftCF (uncurryH4 f)

liftC5
    :: (forall s. C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double -> C.CVar s Double)
    -> CVar
    -> CVar
    -> CVar
    -> CVar
    -> CVar
    -> CVar
liftC5 f = curryH5 $ liftCF (uncurryH5 f)

instance Num CVar where
    (+)    = liftC2 (+)
    (*)    = liftC2 (*)
    (-)    = liftC2 (-)
    negate = liftC negate
    abs    = liftC abs
    signum = liftC signum
    fromInteger = constC . fromInteger

instance Fractional CVar where
    recip = liftC recip
    (/)   = liftC2 (/)
    fromRational = constC . fromRational

instance Floating CVar where
    pi      = constC pi
    exp     = liftC exp
    log     = liftC log
    sqrt    = liftC sqrt
    (**)    = liftC2 (**)
    logBase = liftC2 logBase
    sin     = liftC sin
    cos     = liftC cos
    asin    = liftC asin
    acos    = liftC acos
    atan    = liftC atan
    sinh    = liftC sinh
    cosh    = liftC cosh
    asinh   = liftC asinh
    acosh   = liftC acosh
    atanh   = liftC atanh
