{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.Uncertain.Correlated.Internal
  ( CVar(..), dephantom
  , CorrF(..), Corr(..)
  , liftCF
  , constC, liftC, liftC2, liftC3, liftC4, liftC5
  , corrToState
  )
  where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Hople
import           Data.Uncertain
import           Numeric.AD.Mode.Sparse
import qualified Data.IntMap.Strict        as M

data CVar :: * -> * -> * where
    CK :: a -> CVar s a
    CV :: M.Key -> CVar s a
    CF :: Functor f
       => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
       -> f (CVar s a)
       -> CVar s a

dephantom :: CVar s a -> CVar t a
dephantom = \case CK x    -> CK x
                  CV k    -> CV k
                  CF f xs -> CF f (dephantom <$> xs)

data CorrF :: * -> * -> * -> * where
    Cer :: a -> (CVar s a -> b) -> CorrF s a b
    Gen :: Uncert a -> (CVar s a -> b) -> CorrF s a b
    Fun :: Functor f
        => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
        -> f (CVar s a)
        -> (CVar s a -> b)
        -> CorrF s a b
    Rei :: CVar s a
        -> (Uncert a -> b)
        -> CorrF s a b

deriving instance Functor (CorrF s a)

newtype Corr s a b = Corr { corrFree :: Free (CorrF s a) b
                          }
                   deriving (Functor, Applicative, Monad)

deriving instance MonadFree (CorrF s a) (Corr s a)

corrToState
    :: (Monad m, Fractional a)
    => Corr s a b
    -> StateT (M.Key, M.IntMap (Uncert a)) m b
corrToState = iterM go . corrFree
  where
    go = \case
            Cer c next    ->
              next (CK c)
            Gen u next    -> do
              i <- gets fst
              modify $ bimap succ (M.insert i u)
              next (CV i)
            Fun f us next ->
              next $ CF f us
            Rei v next    -> do
              u <- gets (getCVar v . snd)
              next u
    getCVar
        :: forall a s. Fractional a
        => CVar s a
        -> M.IntMap (Uncert a)
        -> Uncert a
    getCVar cv = liftUF (cVarToF cv)
      where
        cVarToF
            :: CVar s a
            -> (forall t. M.IntMap (AD t (Sparse a)) -> AD t (Sparse a))
        cVarToF (CK x)    _  = auto x
        cVarToF (CV k)    us = us M.! k
        cVarToF (CF f cs) us = f (flip cVarToF us <$> cs)

liftCF
    :: (Functor f, Fractional a)
    => (forall t. f (AD t (Sparse a)) -> AD t (Sparse a))
    -> f (CVar s a)
    -> CVar s a
liftCF f cs = CF f cs

constC :: a -> CVar s a
constC = CK

liftC
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
liftC f = curryH1 $ liftCF (uncurryH1 f)

liftC2
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC2 f = curryH2 $ liftCF (uncurryH2 f)

liftC3
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC3 f = curryH3 $ liftCF (uncurryH3 f)

liftC4
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC4 f = curryH4 $ liftCF (uncurryH4 f)

liftC5
    :: Fractional a
    => (forall t. AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a) -> AD t (Sparse a))
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC5 f = curryH5 $ liftCF (uncurryH5 f)

instance Fractional a => Num (CVar s a) where
    (+)    = liftC2 (+)
    (*)    = liftC2 (*)
    (-)    = liftC2 (-)
    negate = liftC negate
    abs    = liftC abs
    signum = liftC signum
    fromInteger = constC . fromInteger

instance Fractional a => Fractional (CVar s a) where
    recip = liftC recip
    (/)   = liftC2 (/)
    fromRational = constC . fromRational

instance Floating a => Floating (CVar s a) where
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
