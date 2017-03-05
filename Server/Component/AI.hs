{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Component.AI
    ( AI
    , AIModifier (..)
    , mkAI, simpleAI, transAI
    , stepAI, runAI
    ) where

import Prelude hiding ((.), id)

import Component.Modifier
import Core.Types
import Core.Monad
import Core.Engine
import qualified Core.ECS as ECS

import Control.Monad.Reader
import Data.Typeable

newtype AI = AI (Fix (ReaderT ARef (Game () Level))) deriving (Typeable, Monoid)

newtype AIModifier = AIMod (Game () Level () -> Game () Level ())

instance ECS.Component AI where
  stock = mempty

instance ECS.Component AIModifier where
  stock = AIMod id

mkAI :: Fix (ReaderT ARef (Game () Level)) -> AI
mkAI = AI

transAI :: (s -> ARef -> Game () Level s) -> s -> AI
transAI f start = AI (Ana (\s -> ReaderT (f s)) start)

simpleAI :: (ARef -> Game () Level ()) -> AI
simpleAI = mkAI . eternal . ReaderT

stepAI :: ARef -> AI -> Game () Level AI
stepAI self (AI f) = AI <$> runReaderT (step f) self

runAI :: ARef -> Game k Level ()
runAI ref = noTurn $ do
    isAlive <- alive <$> access (aref ref)
    (AIMod aiM) <- modified ref
    when isAlive . aiM $ actor ref #= stepAI ref
