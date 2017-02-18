{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Component.Name (Name (..)) where

import Core.ECS (Component, stock)

import Data.Typeable

import Data.Text (Text)
import qualified Data.Aeson as J

newtype Name = Name Text deriving (Typeable, Monoid, Eq)

instance J.ToJSON Name where
  toJSON (Name n) = J.toJSON n

instance Component Name where
  stock = Name "Unnamed"
