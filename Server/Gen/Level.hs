{-# LANGUAGE OverloadedStrings #-}
module Gen.Level where

import Prelude hiding ((.), id)

import Abyss.Item (Item)
import qualified Abyss.Item as Item
import Core.Types
import Core.Monad
import Core.Engine
import Core.DijkstraMap
import qualified Core.ECS as ECS
import qualified Core.BPS as Path
import Component.Activator
import Component.Name

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans.State
import Data.Array.Unboxed
import Data.Maybe (listToMaybe)
import System.Random

data LevelSpec = LevelSpec
    { generator    :: State StdGen (UArray (Row, Col) Char)
    , initializer  :: Char -> ECS.Entity
    , monsterCount :: Int
    , monsterTable :: Table Egg
    , itemCount    :: Int
    , itemTable    :: Table (Item, Dice)
    }
    
isSolid :: Char -> Bool
isSolid ' ' = False
isSolid '*' = False
isSolid _   = True

isSolidNonDoor :: Char -> Bool
isSolidNonDoor '+' = False
isSolidNonDoor c   = isSolid c

defaultInitializer :: Char -> ECS.Entity
defaultInitializer ' ' = setL ECS.lens (Name "Wall") ECS.empty
defaultInitializer '#' = setL ECS.lens (Name "Floor") ECS.empty
defaultInitializer '*' = setL ECS.lens (Name "Ritual Circle") ECS.empty
defaultInitializer '+' = ECS.insert (Name "Door") >>> ECS.insert door $ ECS.empty
defaultInitializer _   = ECS.empty

loadLevel :: LevelSpec -> Game k Level ()
loadLevel spec = do
    layout <- generate (generator spec)

    solid != amap isSolid layout
    opacity != amap isSolid layout
    seen != listArray (bounds layout) (repeat False)
    statics != array (bounds layout) (map (second ((,) <*> initializer spec)) (assocs layout))
    pathGrid != Path.preProcess (amap isSolidNonDoor layout)

    forM_ (assocs layout) $ \(p, c) -> do
        staticChar p != c

    let freeSpots = map fst $ filter ((==) ' ' . snd) (assocs layout)

    playerLoc <- pickNonEmpty freeSpots
    loc <# aref player != playerLoc
    playerDMap != mkDijkstraMap [playerLoc] (amap isSolid layout)
    modifyLevel shadowCast

    -- We don't want to spawn monsters right on top of the player upon
    -- entering the level.
    let monSpots = filter (\p -> manhattan p playerLoc >= 6) freeSpots

    forM_ [1..(itemCount spec)] $ \_ -> do
        itemLoc <- pickNonEmpty freeSpots
        occupier <- getL (weak (floorItem itemLoc)) <$> level
        case occupier of
            Just _ -> return ()
            Nothing -> do
                (item, d) <- rollTable (itemTable spec)
                n <- dice d
                floorItem itemLoc != Just (Item.name item, n)
    
    forM_ [1..(monsterCount spec)] $ \_ -> do
        spawnLoc <- pickNonEmpty monSpots
        occupier <- listToMaybe . living (\a -> a ^. loc == spawnLoc) <$> level
        case occupier of
            Just _  -> return ()
            Nothing -> void . spawn spawnLoc =<< rollTable (monsterTable spec)
