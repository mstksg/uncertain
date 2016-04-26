{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Data.Uncertain.Correlated where

import           Control.Monad.Free
import           Data.Hople
import           Control.Monad.Trans.State
import           Data.Bifunctor
-- import           Data.Functor.Identity
import           Data.Reflection
import           Data.Uncertain
import           Numeric.AD
import           Numeric.AD.Internal.Reverse (Tape)
import           Numeric.AD.Mode.Reverse     (Reverse)
import qualified Data.IntMap.Strict          as M

data CVar :: * -> * -> * where
    CK :: a -> CVar s a
    CV :: M.Key -> CVar s a
    CF :: Functor f
       => (forall t. Reifies t Tape => f (Reverse t a) -> Reverse t a)
       -> f (CVar s a)
       -> CVar s a

data CorrF :: * -> * -> * -> * where
    Cer :: a -> (CVar s a -> b) -> CorrF s a b
    Gen :: Uncert a -> (CVar s a -> b) -> CorrF s a b
    Fun :: Functor f
        => (forall t. Reifies t Tape => f (Reverse t a) -> Reverse t a)
        -> f (CVar s a)
        -> (CVar s a -> b)
        -> CorrF s a b
    Rei :: CVar s a
        -> (Uncert a -> b)
        -> CorrF s a b
    Cor :: CVar s a
        -> CVar s a
        -> (a -> b)
        -> CorrF s a b

deriving instance Functor (CorrF s a)

type Corr s a = Free (CorrF s a)

corrToState
    :: Num a
    => Corr s a b
    -> State (M.Key, M.IntMap (Uncert a)) b
corrToState = iterM $ \case
                        Cer c next    -> do
                          next (CK c)
                        Gen u next    -> do
                          i <- gets fst
                          modify $ bimap succ (M.insert i u)
                          next (CV i)
                        Fun f us next -> do
                          next $ CF f us
                        Rei v next    -> do
                          u <- gets (getCVar v . snd)
                          next u
                        Cor _ _ _     ->
                          undefined
  where
    getCVar
        :: forall a s. Num a
        => CVar s a
        -> M.IntMap (Uncert a)
        -> Uncert a
    getCVar cv = liftUF (cVarToF cv)
      where
        cVarToF
            :: CVar s a
            -> (forall t. Reifies t Tape => M.IntMap (Reverse t a) -> Reverse t a)
        cVarToF (CK x)    _  = auto x
        cVarToF (CV k)    us = us M.! k
        cVarToF (CF f cs) us = f (flip cVarToF us <$> cs)

runCorr :: Num a => Corr s a b -> b
runCorr = flip evalState (0, M.empty) . corrToState

sampleUncert :: Uncert a -> Corr s a (CVar s a)
sampleUncert u = liftF $ Gen u id

getUncert :: CVar s a -> Corr s a (Uncert a)
getUncert v = liftF $ Rei v id

liftCF
    :: (Functor f, Num a)
    => (forall t. Reifies t Tape => f (Reverse t a) -> Reverse t a)
    -> f (CVar s a)
    -> CVar s a
liftCF f cs = CF f cs

liftC
    :: Num a
    => (forall t. Reifies t Tape => Reverse t a -> Reverse t a)
    -> CVar s a
    -> CVar s a
liftC f x = liftCF (\(H1 x') -> f x') (H1 x)

liftC2
    :: Num a
    => (forall t. Reifies t Tape => Reverse t a -> Reverse t a -> Reverse t a)
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC2 f x y = liftCF (\(H2 x' y') -> f x' y') (H2 x y)

liftC3
    :: Num a
    => (forall t. Reifies t Tape => Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a)
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC3 f x y z = liftCF (\(H3 x' y' z') -> f x' y' z') (H3 x y z)

liftC4
    :: Num a
    => (forall t. Reifies t Tape => Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a)
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC4 f x y z a = liftCF (\(H4 x' y' z' a') -> f x' y' z' a') (H4 x y z a)

liftC5
    :: Num a
    => (forall t. Reifies t Tape => Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a -> Reverse t a)
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
    -> CVar s a
liftC5 f x y z a b = liftCF (\(H5 x' y' z' a' b') -> f x' y' z' a' b') (H5 x y z a b)

instance Num a => Num (CVar s a) where
    (+)    = liftC2 (+)
    (*)    = liftC2 (*)
    (-)    = liftC2 (-)
    negate = liftC negate
    abs    = liftC abs
    signum = liftC signum
    fromInteger = CK . fromInteger

instance Fractional a => Fractional (CVar s a) where
    recip = liftC recip
    (/)   = liftC2 (/)
    fromRational = CK . fromRational

instance Floating a => Floating (CVar s a) where
    pi      = CK pi
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

