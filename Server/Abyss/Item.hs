{-# LANGUAGE OverloadedStrings #-}
module Abyss.Item
    ( Item
    , itemName
    , itemDesc
    , itemModifier
    , Inventory (..)
    , longsword
    , longswordP1
    , longswordP2
    , longswordP3
    , leatherArmor
    ) where

import Abyss.Stats
import Core.Types
import Core.Monad
import qualified Core.ECS as ECS

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data Item = Item
    { _name     :: Text
    , _desc     :: Text
    , _modifier :: ECS.Entity -> ECS.Entity
    }

itemName :: Item -> Text
itemName = _name

itemDesc :: Item -> Text
itemDesc = _desc

itemModifier :: Item -> ECS.Entity -> ECS.Entity
itemModifier = _modifier

data Inventory = Inventory (Map Text Int)

instance ECS.Component Inventory where
  stock = Inventory Map.empty

longsword :: Item
longsword = Item "Longsword" "1d6 slashing damage" (swordMod 0)

longswordP1 :: Item
longswordP1 = Item "Longsword +1" "1d6+1 slashing damage. +1 attack." (swordMod 1)

longswordP2 :: Item
longswordP2 = Item "Longsword +2" "1d6+2 slashing damage. +2 attack." (swordMod 2)

longswordP3 :: Item
longswordP3 = Item "Longsword +3" "1d6+3 slashing damage. +3 attack." (swordMod 3)

swordMod :: Int -> ECS.Entity -> ECS.Entity
swordMod n =
    ECS.modify (setL damage [(Roll 1 d6 + Const n, Slashing)])
    >>> ECS.modify (modL attack (+ n))

leatherArmor :: Item
leatherArmor = Item "Leather Armor" "+2 defense. Maximum dex bonus 4" (armor 2 (NotTop 4))

armor :: Int -> Topped Int -> ECS.Entity -> ECS.Entity
armor x y =
    ECS.modify (modL defense (+ x))
    >>> ECS.modify (modL (maxBonus DEX) (min y))
    
