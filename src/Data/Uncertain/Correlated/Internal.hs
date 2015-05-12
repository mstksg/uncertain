{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Uncertain.Correlated.Internal where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.IntMap.Strict               (IntMap, Key, (!))
import Data.Maybe
import Data.Traversable
import Data.Uncertain
import Data.Uncertain.Internal
import qualified Data.IntMap.Strict     as IM

data CVar b = CVPure b
            | CVKey Key
            | CVProp1 (Propagator1 b) (CVar b)
            | CVProp2 (Propagator2 b) (CVar b) (CVar b)

instance Show a => Show (CVar a) where
    show (CVPure b) = "CVar " ++ show b
    show _          = "CVar ??"


type Propagator1 a = (a, a)         -- previous value, variance
                  -> ((a, a), a)    -- new value, variance, covariance to old


type Propagator2 a = (a, a)         -- previous value, variance
                  -> (a, a)         -- previous value, variance
                  -> a              -- covariance
                  -> ((a, a), a, a) -- new value, variance, covariances

data CorrState b = CS { csCount  :: Key
                      , csValMat :: IntMap b
                      , csVarMat :: IntMap (IntMap b)
                      }

emptyCorrState :: Num a => CorrState a
emptyCorrState = CS 0 IM.empty IM.empty

newtype Correlated b a = C { unCorrelated :: State (CorrState b) a }
                       deriving (Functor, Applicative, Monad)

fromUncertain :: Num b => Uncertain b -> Correlated b (CVar b)
fromUncertain (vl :+- vr) = C $ state f
  where
    f (CS ct cvlm cvrm) = (CVKey ct, CS ct' cvlm' cvrm')
      where
        ct'   = ct + 1
        cvlm' = IM.insert ct vl cvlm
        cvrm' = IM.insert ct (IM.singleton ct (vr * vr)) cvrm


instance Num b => Num (Correlated b (CVar b)) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = fromUncertain . certain . fromInteger

instance Num b => Num (CVar b) where
    (+) = CVProp2 prop
      where
        prop (x, vx) (y, vy) cv = ((x + y, vz), cvx, cvy)
          where
            vz  = vx + vy + 2 * cv
            cvx = vx + vz
            cvy = vy + vz

    (*) = CVProp2 prop
      where
        prop (x, vx) (y, vy) cv = ((x * y, vz), cvx, cvy)
          where
            vz  = y * y * vx + x * x * vy + 2 * x * y * cv
            cvx = y * vx + x * cv
            cvy = x * vy + y * cv

    (-) = CVProp2 prop
      where
        prop (x, vx) (y, vy) cv = ((x - y, vz), cvx, cvy)
          where
            vz  = vx + vy - 2 * cv
            cvx = vx - vz
            cvy = vy + vz
    negate = CVProp1 $ \(x, vx) -> ((-x, vx), -vx)
    abs    = CVProp1 $ \(x, vx) -> ((abs x, vx), signum x * vx) -- covariance is 0 when x == 0
    signum = CVProp1 $ \(x, vx) -> ((signum x, 1), 0)  -- how does this even make sense?  think about it.
    fromInteger = CVPure . fromInteger

