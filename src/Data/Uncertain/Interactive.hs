module Data.Uncertain.Interactive where

import Control.Monad.Trans.State.Strict
import Data.IORef
import Data.Uncertain
import Data.Uncertain.Correlated
import Data.Uncertain.Correlated.Internal
import System.IO.Unsafe

globalCorrState :: IORef (CorrState Double)
{-# NOINLINE globalCorrState #-}
globalCorrState = unsafePerformIO (newIORef emptyCorrState)

correlatedIO :: Correlated Double a -> IO a
correlatedIO (C c) = do
    cs <- readIORef globalCorrState
    let (x, cs') = runState c cs
    writeIORef globalCorrState cs'
    return x

evalCVar :: CVar Double -> IO (Uncertain Double)
evalCVar = correlatedIO . marginalize

evalCorrelation :: CVar Double -> CVar Double -> IO Double
evalCorrelation x y = correlatedIO (getCorrelation x y)

evalCovariance :: CVar Double -> CVar Double -> IO Double
evalCovariance x y = correlatedIO (getCovariance x y)


fromUncertainIO :: Uncertain Double -> IO (CVar Double)
fromUncertainIO = correlatedIO . fromUncertain

fromUncertain2IO :: Uncertain Double
                 -> Uncertain Double
                 -> Double
                 -> IO (CVar Double, CVar Double)
fromUncertain2IO x y p = correlatedIO (fromUncertain2 x y p)

fromUncertain3IO :: Uncertain Double
                 -> Uncertain Double
                 -> Uncertain Double
                 -> Double
                 -> Double
                 -> Double
                 -> IO (CVar Double, CVar Double, CVar Double)
fromUncertain3IO x y z pxy pxz pyz = correlatedIO (fromUncertain3 x y z
                                                                  pxy
                                                                  pxz
                                                                  pyz )

-- fromUncertain2 :: Num b
--                => Uncertain b
--                -> Uncertain b
--                -> b
--                -> Correlated b (CVar b, CVar b)
-- fromUncertain2 (x :+- dx) (y :+- dy) p = C $ state f



