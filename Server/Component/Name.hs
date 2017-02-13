{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Component.Name (Name (..)) where

import Core.ECS (Component, stock)

import Data.Typeable

import Data.Text (Text)
import qualified Data.Text as T

newtype Name = Name Text deriving (Typeable, Monoid, Eq)

instance Component Name where
  stock = Name T.empty
