{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Hoples
  ( H1(..)
  , H2(..)
  , H3(..)
  , H4(..)
  , H5(..)
  )
  where

data H1 a = H1 !a
  deriving (Functor, Foldable, Traversable)

data H2 a = H2 !a !a
  deriving (Functor, Foldable, Traversable)

data H3 a = H3 !a !a !a
  deriving (Functor, Foldable, Traversable)

data H4 a = H4 !a !a !a !a
  deriving (Functor, Foldable, Traversable)

data H5 a = H5 !a !a !a !a !a
  deriving (Functor, Foldable, Traversable)

