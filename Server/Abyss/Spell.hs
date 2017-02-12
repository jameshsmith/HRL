{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Abyss.Spell
    ( luckCurse
    , fizzle
    , fizzleEffect
    , every
    ) where

import Prelude hiding ((.), id)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Core.Types
import Core.Monad
import Core.Engine
import Component.AI
import Component.Modifier

every :: Map String Spell 
every = Map.fromList
    [ ("Curse of Misfortune", luckCurse)
    ]

fizzle :: Spell
fizzle = TargetNone $ fizzleEffect

fizzleEffect :: Effect
fizzleEffect = Effect Nothing (Ana (\s -> lift (message s) >> mzero) "*Fizzle*")

luckCurse :: Spell
luckCurse = TargetActor $ \aref -> Effect Nothing (Ana (luckCurse' aref) Nothing)

luckCurse' :: ARef -> Maybe (Int, MRef) -> MaybeT (Game k Level) (Maybe (Int, MRef))
luckCurse' ref s
  | Nothing     <- s = Just . ((,) 9) <$> lift (addModifier (\(AIMod f) -> AIMod (rollMin . f)) ref)
  | Just (n, m) <- s
  , n == 0           = lift (removeModifier m) >> mzero
  | Just (n, m) <- s = return (Just (n - 1, m))
      
