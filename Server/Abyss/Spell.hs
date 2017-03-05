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
    , timereversal
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
import Control.Monad.Free
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

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
    , timereversal
    ]

fizzle :: Spell
fizzle = Spell "Fizzle" (None fizzleEffect)

fizzleEffect :: Effect
fizzleEffect = Effect Nothing (Ana (\s -> lift (message s) >> mzero) "*Fizzle*")

simpleEffect :: Game () Level () -> Effect
simpleEffect m = Effect Nothing (Ana (\_ -> lift m >> mzero) ())

fixFree :: Monad m => Free m () -> Fix (MaybeT m)
fixFree = Ana go
 where
   go (Pure ())   = mzero
   go (Free cont) = lift cont

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
  | Nothing     <- s =
      Just . ((,) 9) <$> lift (addModifier (\(AIModifier f) -> AIModifier (rollMin . f)) ref)
  | Just (n, m) <- s
  , n == 0           = lift (removeModifier m) >> mzero
  | Just (n, m) <- s = return (Just (n - 1, m))

timereversal :: Spell
timereversal = Spell "Time Reversal" (None (Effect Nothing (fixFree timereversal')))

timereversal' :: Free (Game k Level) ()
timereversal' = do
    l <- liftF (do { l' <- level; message "Temporal reset in 10"; return l' })
    forM_ [9, 8..1] $ \n ->
        liftF (message ("Temporal reset in " <> T.pack (show n)))
    liftF (unsafeWeakLens id != l)
