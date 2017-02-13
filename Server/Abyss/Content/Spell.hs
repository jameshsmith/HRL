{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Abyss.Content.Spell
    ( luckCurse
    , fizzle
    , fizzleEffect
    , every
    ) where

import Prelude hiding ((.), id)

import Core.Types
import Core.Monad
import Core.Engine
import Component.AI
import Component.Modifier

import Control.Arrow ((&&&))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

every :: Map Text Spell 
every = Map.fromList $ map (spellName &&& id)
    [ luckCurse
    , fizzle
    ]

fizzle :: Spell
fizzle = TargetNone "Fizzle" fizzleEffect

fizzleEffect :: Effect
fizzleEffect = Effect Nothing (Ana (\s -> lift (message s) >> mzero) "*Fizzle*")

luckCurse :: Spell
luckCurse = TargetActor "Curse of Misfortune" $ \aref ->
    Effect Nothing (Ana (luckCurse' aref) Nothing)

luckCurse' :: ARef -> Maybe (Int, MRef) -> MaybeT (Game k Level) (Maybe (Int, MRef))
luckCurse' ref s
  | Nothing     <- s = Just . ((,) 9) <$> lift (addModifier (\(AIMod f) -> AIMod (rollMin . f)) ref)
  | Just (n, m) <- s
  , n == 0           = lift (removeModifier m) >> mzero
  | Just (n, m) <- s = return (Just (n - 1, m))
      
