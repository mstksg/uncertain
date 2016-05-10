{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE LambdaCase     #-}

module Data.Connected where

import Type.Family.Nat
import Data.Type.Vector
import Data.Type.Combinator

infixr 4 :*~

data ConnT :: N -> (* -> *) -> * -> * where
    ØC    :: ConnT 'Z f a
    (:*~) :: VT n f a -> ConnT n f a -> ConnT ('S n) f a

elimConnT
    :: p 'Z
    -> (forall m. VT m f a -> p m -> p ('S m))
    -> ConnT n f a
    -> p n
elimConnT z s = \case 
    ØC      -> z
    v :*~ c -> s v (elimConnT z s c)

type Conn n = ConnT n I

