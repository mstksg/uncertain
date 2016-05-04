{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Uncertain.Correlated.Interactive
  ( CVar
  , sampleUncert
  , sampleExact
  , resolveUncert
  )
  where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Tuple
import           Data.Uncertain
import           Data.Uncertain.Correlated.Internal hiding (CVar(..))
import           System.IO.Unsafe                          (unsafePerformIO)
import qualified Data.IntMap.Strict                        as M
import qualified Data.Uncertain.Correlated                 as C


newtype CVar = CV (forall s. C.CVar s Double)

globalCorrMap :: IORef (M.Key, M.IntMap (Uncert Double))
{-# NOINLINE globalCorrMap #-}
globalCorrMap = unsafePerformIO $ newIORef (0, M.empty)

runCorrIO :: (forall s. Corr s Double a) -> IO a
runCorrIO c = do
    atomicModifyIORef' globalCorrMap (swap . runState (corrToState c))

sampleUncert :: Uncert Double -> IO CVar
sampleUncert u = runCorrIO $ wrapCV (C.sampleUncert u)

sampleExact :: Double -> IO CVar
sampleExact d = runCorrIO $ wrapCV (C.sampleExact d)

resolveUncert :: CVar -> IO (Uncert Double)
resolveUncert = \case CV v -> runCorrIO $ C.resolveUncert v

wrapCV :: (forall t. Corr t Double (C.CVar t Double)) -> Corr s Double CVar
wrapCV = fmap (\v -> CV (dephantom v))
