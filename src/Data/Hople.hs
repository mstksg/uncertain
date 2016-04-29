{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Hople
  ( H1(..)
  , H2(..)
  , H3(..)
  , H4(..)
  , H5(..)
  , curryH1, curryH2, curryH3, curryH4, curryH5
  , uncurryH1, uncurryH2, uncurryH3, uncurryH4, uncurryH5
  )
  where

data H1 a = H1 !a
  deriving (Functor, Foldable, Traversable, Show)

data H2 a = H2 !a !a
  deriving (Functor, Foldable, Traversable, Show)

data H3 a = H3 !a !a !a
  deriving (Functor, Foldable, Traversable, Show)

data H4 a = H4 !a !a !a !a
  deriving (Functor, Foldable, Traversable, Show)

data H5 a = H5 !a !a !a !a !a
  deriving (Functor, Foldable, Traversable, Show)

curryH1 :: (H1 a -> a) -> a -> a
curryH1 f x = f (H1 x)

curryH2 :: (H2 a -> a) -> a -> a -> a
curryH2 f x y = f (H2 x y)

curryH3 :: (H3 a -> a) -> a -> a -> a -> a
curryH3 f x y z = f (H3 x y z)

curryH4 :: (H4 a -> a) -> a -> a -> a -> a -> a
curryH4 f x y z a = f (H4 x y z a)

curryH5 :: (H5 a -> a) -> a -> a -> a -> a -> a -> a
curryH5 f x y z a b = f (H5 x y z a b)

uncurryH1 :: (a -> a) -> H1 a -> a
uncurryH1 f (H1 x) = f x

uncurryH2 :: (a -> a -> a) -> H2 a -> a
uncurryH2 f (H2 x y) = f x y

uncurryH3 :: (a -> a -> a -> a) -> H3 a -> a
uncurryH3 f (H3 x y z) = f x y z

uncurryH4 :: (a -> a -> a -> a -> a) -> H4 a -> a
uncurryH4 f (H4 x y z a) = f x y z a

uncurryH5 :: (a -> a -> a -> a -> a -> a) -> H5 a -> a
uncurryH5 f (H5 x y z a b) = f x y z a b


