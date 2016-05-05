module Data.Uncertain.Correlated
  ( CVar
  , Corr
  , runCorr
  , sampleUncert, sampleExact
  , resolveUncert
  , liftCF
  , constC, liftC, liftC2, liftC3, liftC4, liftC5
  )
  where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Data.Uncertain
import           Data.Uncertain.Correlated.Internal
import qualified Data.IntMap.Strict                 as M

runCorr :: Fractional a => Corr s a b -> b
runCorr = flip evalState (0, M.empty) . corrToState

sampleUncert :: Uncert a -> Corr s a (CVar s a)
sampleUncert u = liftF $ Gen u id

sampleExact :: a -> Corr s a (CVar s a)
sampleExact x = liftF $ Cer x id

resolveUncert :: CVar s a -> Corr s a (Uncert a)
resolveUncert v = liftF $ Rei v id

