{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Abyss.Monster where

import Prelude hiding ((.), id)

import Abyss.Stats
import Component.Activator
import Component.AI
import Component.Name
import qualified Core.ECS as ECS
import Core.Types
import Core.Monad
import Core.Engine
import Core.DijkstraMap

import Control.Monad
import Data.Array.IArray
import Data.Maybe (listToMaybe, maybeToList)

import qualified Data.Text as T

monMove :: (Row, Col) -> ARef -> [Dir4] -> Game () Level ()
monMove _   _    []     = return ()
monMove pos self (d:ds) = do
    occupier <- listToMaybe . living (\a -> a ^. loc == move4 d pos) <$> level
    blocked  <- access (ix (move4 d pos) <# solid)
    door     <- (==) '+' <$> access (staticChar (move4 d pos)) 
    case occupier of
        Just mon | mon == player   -> self `meleeAttack` mon
        Just _                     -> monMove pos self ds
        Nothing  | blocked && door -> self `activate` move4 d pos
        Nothing  | blocked         -> monMove pos self ds
        Nothing                    -> loc <# aref self != move4 d pos

approachPlayer :: (Row, Col) -> ARef -> Game () Level ()
approachPlayer pos self = do
    moves <- downhill pos <$> access playerDMap
    d <- pick N4 [E4, S4, W4]
    monMove pos self (moves ++ [d])
    

zombie :: ECS.Entity
zombie =
    ECS.insert (simpleAI shamble)
    >>> ECS.insert (Name "Zombie")
    >>> ECS.modify (setL defense 5)
    >>> ECS.modify (setL (attribute CON) 7)
    >>> ECS.modify (setL (attribute INT) 1)
    >>> ECS.modify (setL damage [(Roll 1 d4, Bashing), (Roll 1 (0, 1), Necrotic)])
    $ ECS.empty

shamble :: ARef -> Game () Level ()
shamble self = do
    willMove <- coin
    when willMove $ do
        pos <- access (loc <# aref self)
        approachPlayer pos self

skeleton :: ECS.Entity
skeleton =
    ECS.insert (transAI hunterAI HunterSleep)
    >>> ECS.insert (Name "Skeleton")
    >>> ECS.modify (setL defense 6)
    >>> ECS.modify (setL (attribute CON) 8)
    >>> ECS.modify (setL (attribute STR) 9)
    >>> ECS.modify (setL (attribute INT) 4)
    >>> ECS.modify (setL damage [(Roll 1 d6, Piercing)])
    $ ECS.empty

data HunterState = HunterSleep
                 | HunterHunt [(Row, Col)]
                 | HunterAttack (Row, Col)

hunterAI :: HunterState -> ARef -> Game () Level HunterState                
hunterAI HunterSleep self = do
    pos <- access (loc <# aref self)
    spotted <- (! pos) . visible <$> level
    if not spotted
        then return HunterSleep
        else do
            playerPos <- access (loc <# aref player)
            hunterAI (HunterAttack playerPos) self

hunterAI (HunterAttack playerPos) self = do
    pos <- access (loc <# aref self)
    spotted <- (! pos) . visible <$> level
    if spotted
        then do
            -- Move towards the player
            approachPlayer pos self
            playerPos' <- access (loc <# aref player)
            return (HunterAttack playerPos')
        else do
            -- Try to pathfind towards the last known location
            huntPath <- pathfind pos playerPos
            message (T.pack (show huntPath))
            case huntPath of
                Nothing -> return HunterSleep
                Just path -> hunterAI (HunterHunt path) self

hunterAI (HunterHunt []) _ = return HunterSleep

hunterAI (HunterHunt (p : path)) self = do
    pos <- access (loc <# aref self)
    spotted <- (! pos) . visible <$> level
    if | spotted -> do
           -- Move towards the player
           approachPlayer pos self
           playerPos' <- access (loc <# aref player)
           return (HunterAttack playerPos') 
       | pos == p -> hunterAI (HunterHunt path) self
       | otherwise -> do
           -- Keep pathfinding towards last known location
           monMove pos self (maybeToList (direction pos p))
           return (HunterHunt (p : path))
