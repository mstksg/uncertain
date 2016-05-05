module Data.Uncertain.Correlated.Interactive
  ( Corr, runCorrIO
  , CVar
  , sampleUncert
  , sampleExact
  , resolveUncert
  , constC, liftC, liftC2, liftC3, liftC4, liftC5, liftCF
  )
  where

import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Tuple
import           Data.Uncertain
import           Data.Uncertain.Correlated.Internal
import           System.IO.Unsafe                   (unsafePerformIO)
import qualified Data.IntMap.Strict                 as M
import qualified Data.Uncertain.Correlated          as C

type CVarIO = CVar RealWorld Double

globalCorrMap :: IORef (M.Key, M.IntMap (Uncert Double))
{-# NOINLINE globalCorrMap #-}
globalCorrMap = unsafePerformIO $ newIORef (0, M.empty)

runCorrIO :: Corr RealWorld Double a -> IO a
runCorrIO c = atomicModifyIORef' globalCorrMap
                                 (swap . runState (corrToState c))

sampleUncert :: Uncert Double -> IO CVarIO
sampleUncert u = runCorrIO $ C.sampleUncert u

sampleExact :: Double -> IO CVarIO
sampleExact d = runCorrIO $ C.sampleExact d

resolveUncert :: CVarIO -> IO (Uncert Double)
resolveUncert v = runCorrIO $ C.resolveUncert v
