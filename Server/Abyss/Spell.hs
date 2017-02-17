{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Abyss.Spell
    ( Target (..)
    , name
    , target
    , every
    , fizzle
    , fizzleEffect
    , luckCurse
    , fireblast
    ) where

import Prelude hiding ((.), id)

import Core.Types
import Core.Monad
import Core.Engine
import Component.AI
import Component.Modifier
import Component.Name
import Abyss.Stats

import Control.Arrow ((&&&))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

data Spell = Spell Text Target

data Target =
      Actor (ARef -> Effect)
    | Location ((Row, Col) -> Effect)
    | None Effect

name :: Spell -> Text
name (Spell s _) = s

target :: Spell -> Target
target (Spell _ t) = t

every :: Map Text Spell 
every = Map.fromList $ map (name &&& id)
    [ luckCurse
    , fizzle
    , fireblast
    ]

fizzle :: Spell
fizzle = Spell "Fizzle" (None fizzleEffect)

fizzleEffect :: Effect
fizzleEffect = Effect Nothing (Ana (\s -> lift (message s) >> mzero) "*Fizzle*")

simpleEffect :: Game () Level () -> Effect
simpleEffect m = Effect Nothing (Ana (\_ -> lift m >> mzero) ())

fireblast :: Spell
fireblast = Spell "Fireblast" . Actor $ \aref -> simpleEffect (fireblast' aref)

fireblast' :: ARef -> Game () Level ()
fireblast' victim = do
    dam <- rollDamage [(Roll 1 d6, Fire)] =<< modified victim
    actor victim %= (+ Hurt (sum (map fst dam)))
    (Name vName) <- modified victim
    message ("Fireblast hit " <> vName <> " for " <> prettyDamage dam)
                                                              
luckCurse :: Spell
luckCurse = Spell "Curse of Misfortune" . Actor $ \aref ->
    Effect Nothing (Ana (luckCurse' aref) Nothing)

luckCurse' :: ARef -> Maybe (Int, MRef) -> MaybeT (Game k Level) (Maybe (Int, MRef))
luckCurse' ref s
  | Nothing     <- s = Just . ((,) 9) <$> lift (addModifier (\(AIMod f) -> AIMod (rollMin . f)) ref)
  | Just (n, m) <- s
  , n == 0           = lift (removeModifier m) >> mzero
  | Just (n, m) <- s = return (Just (n - 1, m))
      
