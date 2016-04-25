{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Uncertain where

import Data.Data
import GHC.Generics


data Uncert a = U { uMean :: a
                  , uVar  :: a
                  }
  deriving (Data, Typeable, Generic, Generic1)


