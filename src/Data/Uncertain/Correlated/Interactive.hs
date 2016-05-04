{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase                #-}

module Data.Uncertain.Correlated.Interactive where

import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Tuple
import           Data.Uncertain
import           Data.Uncertain.Correlated.Internal hiding (CVar(..))
import           System.IO.Unsafe                   (unsafePerformIO)
import qualified Data.IntMap.Strict                 as M
import qualified Data.Uncertain.Correlated          as C


newtype CVar a = CV (forall s. C.CVar s a)

globalCorrMap :: IORef (M.Key, M.IntMap (Uncert Double))
{-# NOINLINE globalCorrMap #-}
globalCorrMap = unsafePerformIO $ newIORef (0, M.empty)

runCorrIO :: (forall s. Corr s Double a) -> IO a
runCorrIO c = do
    atomicModifyIORef' globalCorrMap (swap . runState (corrToState c))

-- sampleUncert :: Uncert Double -> IO (CVar s Double)
-- sampleUncert = runCorrIO . C.sampleUncert

-- sampleExact :: Double -> IO (CVar Double)
-- sampleExact d = runCorrIO (C.sampleExact d)

resolveUncert :: CVar Double -> IO (Uncert Double)
resolveUncert = \case CV v -> runCorrIO $ C.resolveUncert v
