{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Component.Name (Name (..)) where

import Core.ECS (Component, stock)

import Data.Typeable

newtype Name = Name String deriving (Typeable, Monoid, Eq)

instance Component Name where
  stock = Name ""
