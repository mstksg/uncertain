module Data.Uncertain.Correlated (
    CVar
  , Correlated
  , fromUncertain
  , fromUncertain2
  , fromUncertain3
  , marginalize
  , marginalize2
  , getCorrelation
  , getCovariance
  , getCorrelated
  , getCorrelateds
  , getCorrelatedT
  , normalizeCVar
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.IntMap.Strict                 (IntMap, Key, (!))
import Data.Maybe
import Data.Traversable
import Data.Uncertain
import Data.Uncertain.Correlated.Internal
import Data.Uncertain.Internal
import qualified Data.IntMap.Strict       as IM

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
marginalize = fmap f . getCVar
  where
    f (_, x, vx) = x :+- sqrt vx

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
getCorrelatedT ccs = evalState st emptyCorrState
  where
    C st = traverse marginalize =<< ccs

marginalize2 :: Floating b
             => CVar b
             -> CVar b
             -> Correlated b (Uncertain b, Uncertain b, b)
marginalize2 cx cy = f <$> getCVar2 cx cy
  where
    f ((_,x,vx),(_,y,vy),p) = ( x :+- sqrt vx
                              , y :+- sqrt vy
                              , p
                              )

setCorrelation :: CVar b -> CVar b -> b -> Correlated b ()
setCorrelation cx cy p = undefined

getCVar :: Num b
        => CVar b
        -> Correlated b (Key, b, b)
getCVar cx = do
    CVKey k <- normalizeCVar cx
    C . gets $ \(CS _ vl vr) -> let x  = fromMaybe (error $ "Database error on key " ++ show k)
                                       $ IM.lookup k vl
                                    vx = fromMaybe 0 $ dblu k k vr
                                in  (k, x, vx)

getCVar2 :: Num b
         => CVar b
         -> CVar b
         -> Correlated b ((Key, b, b), (Key, b, b), b)
getCVar2 cx cy = do
    CVKey kx <- normalizeCVar cx
    CVKey ky <- normalizeCVar cy
    C . gets $ \(CS _ vl vr) -> let x  = fromMaybe (error $ "Database error on key " ++ show kx)
                                       $ IM.lookup kx vl
                                    y  = fromMaybe (error $ "Database error on key " ++ show kx)
                                       $ IM.lookup ky vl
                                    vx = fromMaybe 0 $ dblu kx kx vr
                                    vy = fromMaybe 0 $ dblu ky ky vr
                                    cv = fromMaybe 0 $ dblu kx ky vr
                                in  ((kx, x, vx), (ky, y, vy), cv)

getCorrelation :: Floating b
               => CVar b
               -> CVar b
               -> Correlated b b
getCorrelation cx cy = f <$> getCVar2 cx cy
  where
    f ((_,_,vx), (_,_,vy), p) = p / sqrt (vx * vy)

getCovariance :: Num b
              => CVar b
              -> CVar b
              -> Correlated b b
getCovariance cx cy = thrd <$> getCVar2 cx cy
  where
    thrd (_,_,z) = z

-- major flaw here: also have to update all variables that were correlated
-- with the original keys to the new covarinces
normalizeCVar :: Num b => CVar b -> Correlated b (CVar b)
normalizeCVar cv = case cv of
    CVPure u -> C . state $ \cs@(CS i iv _) ->
        (CVKey i, cs { csCount = i + 1
                     , csValMat = IM.insert i u iv
                     })
    CVKey k  -> return cv
    CVProp1 prop cx    -> do
        (k, x, vx) <- getCVar cx
        C . state $ \(CS i vl vr) -> let ((y, vy), cv) = prop (x, vx)
                                         i'      = i + 1
                                         vl'     = IM.insert i y vl
                                         vr'     = IM.insertWith IM.union k (IM.singleton i cv)
                                                 . IM.insertWith IM.union i (IM.fromList [(i, vy), (k, cv)])
                                                 $ vr
                                     in  (CVKey i, CS i' vl' vr')
    CVProp2 prop cx cy -> do
        ((kx, x, vx), (ky, y, vy), cv) <- getCVar2 cx cy
        C . state $ \(CS i vl vr) -> let ((z,vz),cvx,cvy) = prop (x, vx) (y, vy) cv
                                         i'  = i + 1
                                         vl' = IM.insert i z vl
                                         vr' = IM.insertWith IM.union ky (IM.singleton i cvy)
                                             . IM.insertWith IM.union kx (IM.singleton i cvx)
                                             . IM.insertWith IM.union i (IM.fromList [(i, vz),(kx, cvx),(ky,cvy)])
                                             $ vr
                                     in  (CVKey i, CS i' vl' vr')

dblu :: Key -> Key -> IntMap (IntMap a) -> Maybe a
dblu k1 k2 imim = IM.lookup k2 =<< IM.lookup k1 imim

-- test :: Floating b => [Uncertain b]
-- test = getCorrelateds $ do
--     (x,y,z) <- fromUncertain3 (11 +/- 4) (15 +/- 6) (9 +/- 2) 0.9 (-0.7) 0.2
--     return [x + y, x - y, x + z * y, (x * y) * z, x * (y * z), 2 * x, x + x]
