{-# LANGUAGE OverloadedStrings #-}
module Gen.Level where

import Prelude hiding ((.), id)

import Abyss.Stats
import Core.Types
import Core.Monad
import Core.Engine
import Core.DijkstraMap
import qualified Core.ECS as ECS
import Component.Activator
import Component.AI
import Component.Name

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans.State
import Data.Array.Unboxed
import Data.Maybe (listToMaybe)
import System.Random
import Data.Text (Text)

zombie :: ECS.Entity
zombie =
  ECS.insert (simpleAI shamble)
  >>> ECS.insert (Name "Zombie")
  >>> ECS.modify (setL defense 5)
  >>> ECS.modify (setL (attribute CON) 7)
  >>> ECS.modify (setL damage [(Roll 1 d4, Bashing), (Roll 1 (0, 1), Necrotic)])
  $ ECS.empty

shamble :: ARef -> Game () Level ()
shamble self = do
    willMove <- coin
    when willMove $ do
        pos <- access (loc <# aref self)
        moves <- downhill pos <$> access playerDMap
        d <- pick N4 [E4, S4, W4]
        monMove pos self (moves ++ [d])

monMove :: (Row, Col) -> ARef -> [Dir4] -> Game () Level ()
monMove _   _    []     = return ()
monMove pos self (d:ds) = do
    occupier <- listToMaybe . living (\a -> a ^. loc == move4 d pos) <$> level
    blocked  <- access (ix (move4 d pos) <# solid)
    case occupier of
        Just mon | mon == player -> self `meleeAttack` mon
        Just _                   -> monMove pos self ds
        Nothing  | blocked       -> monMove pos self ds
        Nothing                  -> loc <# aref self != move4 d pos

data LevelSpec = LevelSpec
    { generator    :: State StdGen (UArray (Row, Col) Char)
    , initializer  :: Char -> ECS.Entity
    , monsterCount :: Int
    , monsterTable :: Table Egg
    , itemCount    :: Int
    , itemTable    :: Table (Text, Dice)
    }
    
isSolid :: Char -> Bool
isSolid ' ' = False
isSolid '*' = False
isSolid _   = True

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

    forM_ [1..50] $ \_ -> do
        spawnLoc <- pickNonEmpty monSpots
        occupier <- listToMaybe . living (\a -> a ^. loc == spawnLoc) <$> level
        case occupier of
            Just _  -> return ()
            Nothing -> do
                void $ spawn spawnLoc (Egg 'Z' White zombie)
