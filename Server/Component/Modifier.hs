module Component.Modifier
    ( MRef
    , addModifier
    , addRawModifier
    , removeModifier
    , selfModify
    , modified
    ) where

import Core.Types
import Core.Monad
import Core.Engine
import qualified Core.ECS as ECS

import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap

newtype Modifiers = Mods (IntMap (ECS.Entity -> ECS.Entity))

data MRef = MRef !ARef !Int

instance ECS.Component Modifiers where
  stock = Mods IMap.empty

addRawModifier :: (ECS.Entity -> ECS.Entity) -> ARef -> Game k Level MRef
addRawModifier f ref = do
    uid <- uniqueInt
    actor ref %= (\(Mods ms) -> Mods $ IMap.insert uid f ms)
    return (MRef ref uid)

addModifier :: ECS.Component s => (s -> s) -> ARef -> Game k Level MRef
addModifier m ref = do
    uid <- uniqueInt
    actor ref %= (\(Mods ms) -> Mods $ IMap.insert uid (ECS.modify m) ms)
    return (MRef ref uid)

removeModifier :: MRef -> Game k Level ()
removeModifier (MRef ref n) = do
    actor ref %= (\(Mods ms) -> Mods $ IMap.delete n ms)

selfModify :: ECS.Entity -> ECS.Entity
selfModify ent =
    IMap.foldr ($) ent ms
  where
    (Mods ms) = getL ECS.component ent

modified :: ECS.Component s => ARef -> Game k Level s
modified ref = do
    ent <- access (ECS.entity <# aref ref)
    let (Mods ms) = getL ECS.component ent
    return $ getL ECS.lens (IMap.foldr ($) ent ms)
