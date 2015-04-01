{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Uncertain.Correlated (
    CVar
  , Correlated
  , fromUncertain
  , fromUncertain2
  , fromUncertain3
  , marginalize
  , getCorrelated
  , getCorrelateds
  , getCorrelatedT
  , normalizeCVar
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.IntMap.Strict               (IntMap, Key, (!))
import Data.Uncertain
import Control.Arrow
import Data.Traversable
import Data.Functor.Identity
import Data.Uncertain.Internal
import qualified Data.IntMap.Strict     as IM

data CVar b = CVPure b
            | CVKey Key
            | CVProp1 ((b, b) -> (b, b)) (CVar b)
            | CVProp2 (Propagator2 b) (CVar b) (CVar b)

type Propagator2 a = (a, a) -> (a, a) -> a -> ((a, a), a, a)

data CorrState b = CS { csCount  :: Key
                      , csValMat :: IntMap b
                      , csVarMat :: IntMap (IntMap b)
                      }

newtype Correlated b a = C { unCorrelated :: State (CorrState b) a }
                       deriving (Functor, Applicative, Monad)


instance Num b => Num (Correlated b (CVar b)) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = fromUncertain . certain . fromInteger

fromUncertain :: Num b => Uncertain b -> Correlated b (CVar b)
fromUncertain (vl :+- vr) = C $ state f
  where
    f (CS ct cvlm cvrm) = (CVKey ct, CS ct' cvlm' cvrm')
      where
        ct'   = ct + 1
        cvlm' = IM.insert ct vl cvlm
        cvrm' = IM.insert ct (IM.singleton ct (vr * vr)) cvrm

fromUncertain2 :: Num b
               => Uncertain b
               -> Uncertain b
               -> b
               -> Correlated b (CVar b, CVar b)
fromUncertain2 (x :+- dx) (y :+- dy) p = C $ state f
  where
    f (CS i1 cvlm cvrm) = ((CVKey i1, CVKey i2), CS i3 cvlm' cvrm')
      where
        i2    = i1 + 1
        i3    = i2 + 1
        cvlm' = IM.insert i2 y
              . IM.insert i1 x
              $ cvlm
        cov   = p * dx * dy         -- p should be between -1 and 1
        cvrm' = IM.insertWith IM.union i2 (IM.fromList [(i2, dy * dy), (i1, cov)])
              . IM.insertWith IM.union i1 (IM.fromList [(i1, dx * dx), (i2, cov)])
              $ cvrm

fromUncertain3 :: Num b
               => Uncertain b
               -> Uncertain b
               -> Uncertain b
               -> b
               -> b
               -> b
               -> Correlated b (CVar b, CVar b, CVar b)
fromUncertain3 (x :+- dx) (y :+- dy) (z :+- dz) pxy pxz pyz = C $ state f
  where
    f (CS i1 cvlm cvrm) = ((CVKey i1, CVKey i2, CVKey i3), CS i4 cvlm' cvrm')
      where
        i2 = i1 + 1
        i3 = i2 + 1
        i4 = i3 + 1
        cxy = pxy * dx * dy
        cxz = pxz * dx * dz
        cyz = pyz * dy * dz
        cvlm' = IM.insert i3 z
              . IM.insert i2 y
              . IM.insert i1 x
              $ cvlm
        cvrm' = IM.insertWith IM.union i3 (IM.fromList [(i3, dz * dz), (i1, cxz), (i2, cyz)])
              . IM.insertWith IM.union i2 (IM.fromList [(i2, dy * dy), (i1, cxy), (i3, cyz)])
              . IM.insertWith IM.union i1 (IM.fromList [(i1, dx * dx), (i2, cxy), (i3, cxz)])
              $ cvrm

marginalize :: Floating b
            => CVar b
            -> Correlated b (Uncertain b)
marginalize cv = do
    CVKey k <- normalizeCVar cv
    C . gets $ \(CS _ vl vr) -> (vl ! k) :+- maybe 0 sqrt (dblu k k vr)

getCorrelated :: Floating b
              => Correlated b (CVar b)
              -> Uncertain b
getCorrelated = runIdentity . getCorrelatedT . fmap Identity

getCorrelateds :: Floating b
               => Correlated b [CVar b]
               -> [Uncertain b]
getCorrelateds = getCorrelatedT

getCorrelatedT :: (Floating b, Traversable f)
               => Correlated b (f (CVar b))
               -> f (Uncertain b)
getCorrelatedT ccs = evalState st (CS 0 IM.empty IM.empty)
  where
    C st = traverse marginalize =<< ccs

-- major flaw here: also have to update all variables that were correlated
-- with the original keys to the new covarinces
normalizeCVar :: Num b => CVar b -> Correlated b (CVar b)
normalizeCVar cv = case cv of
    CVPure u -> C . state $ \cs@(CS i iv _) ->
        (CVKey i, cs { csCount = i + 1
                     , csValMat = IM.insert i u iv
                     })
    CVKey k  -> return cv
    CVProp2 prop cx cy -> do
        CVKey kx <- normalizeCVar cx
        CVKey ky <- normalizeCVar cy
        C . state $ \(CS i vl vr) -> let x   = vl ! kx
                                         y   = vl ! ky
                                         vx  = fromMaybe 0 $ dblu kx kx vr
                                         vy  = fromMaybe 0 $ dblu ky ky vr
                                         cv  = fromMaybe 0 $ dblu kx ky vr
                                         ((z,vz),cvx,cvy) = prop (x, vx) (y, vy) cv
                                         i'  = i + 1
                                         vl' = IM.insert i z vl
                                         vr' = IM.insertWith IM.union ky (IM.singleton i cvy)
                                             . IM.insertWith IM.union kx (IM.singleton i cvx)
                                             . IM.insertWith IM.union i (IM.fromList [(i, vz),(kx, cvx),(ky,cvy)])
                                             $ vr
                                     in  (CVKey i, CS i' vl' vr')

dblu :: Key -> Key -> IntMap (IntMap a) -> Maybe a
dblu k1 k2 imim = IM.lookup k2 =<< IM.lookup k1 imim

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
    negate = CVProp1 (first negate)
    abs    = CVProp1 (first abs)
    signum = CVProp1 (first signum)     -- really?
    fromInteger = CVPure . fromInteger

-- test :: Floating b => [Uncertain b]
-- test = getCorrelateds $ do
--     (x,y,z) <- fromUncertain3 (11 +/- 4) (15 +/- 6) (9 +/- 2) 0.9 (-0.7) 0.2
--     return [x + y, x - y, x + z * y, (x * y) * z, x * (y * z), 2 * x, x + x]
