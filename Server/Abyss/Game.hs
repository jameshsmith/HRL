{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Abyss.Game (initGame, Action (..)) where

import Prelude hiding ((.), id)

import Abyss.Stats
import Abyss.Item (Item)
import qualified Abyss.Item as Item
import qualified Abyss.Spell as Spell
import Core.Types
import Core.Engine
import Core.Monad
import Core.DijkstraMap
import Component.Activator
import Component.AI
import Component.Name
import Component.Modifier
import Gen.Dungeon
import Gen.Level

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Serialize as S
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

bump :: ARef -> Dir4 -> Game k Level ()
bump self dir = do
    l        <- access (loc <# aref self)
    occupier <- listToMaybe . living (\a -> a ^. loc == move4 dir l) <$> level
    blocked  <- access (ix (move4 dir l) <# solid)
    case occupier of
        Just mon -> self `meleeAttack` mon
        Nothing -> do
            when blocked $ self `activate` move4 dir l
            loc <# aref self != if blocked then l else move4 dir l

data Action = Move Dir4
            | Skip
            | Activate Dir4
            | Cast Text (Row, Col)
            | Pickup (Row, Col)
            deriving Show

parseDir4 :: MonadPlus m => Text -> m Dir4
parseDir4 "N" = pure N4
parseDir4 "E" = pure E4
parseDir4 "S" = pure S4
parseDir4 "W" = pure W4
parseDir4 _ = mzero

parseSkip :: MonadPlus m => Text -> m Action
parseSkip t
  | t == "skip" = return Skip
  | otherwise = mzero

instance J.FromJSON Action where
  parseJSON v = msum
      [ J.withObject "move" (\obj -> fmap Move (parseDir4 =<< obj .: "move")) v
      , J.withText "skip" parseSkip v
      , J.withObject "activate" (\obj -> fmap Activate (parseDir4 =<< obj .: "activate")) v
      , flip (J.withObject "cast") v $ \obj -> do
          n <- obj .: "name"
          r <- obj .: "row"
          c <- obj .: "col"
          return (Cast n (r, c))
      , flip (J.withObject "pickup") v $ \obj -> do
          r <- obj .: "row"
          c <- obj .: "col"
          return (Pickup (r, c))
      ]

putText :: S.Putter Text
putText txt = S.put (BS.length bs) >> S.putByteString bs
  where
    bs = T.encodeUtf8 txt

getText :: S.Get Text
getText = fmap T.decodeUtf8 (S.getByteString =<< S.get)

instance S.Serialize Action where
  put (Move d)     = S.putWord8 0x00 >> S.put d
  put Skip         = S.putWord8 0x01
  put (Activate d) = S.putWord8 0x02 >> S.put d
  put (Cast s l)   = S.putWord8 0x03 >> putText s >> S.put l
  put (Pickup l)   = S.putWord8 0x04 >> S.put l

  get = S.getWord8 >>= \case
      0x00 -> Move <$> S.get
      0x01 -> return Skip
      0x02 -> Activate <$> S.get
      0x03 -> Cast <$> getText <*> S.get
      0x04 -> Pickup <$> S.get
      _    -> error "Invalid action!"

rooms :: [(Int, Int)]
rooms = [(6,7),(6,5),(4,4),(3,4),(4,2)] ++ concat (repeat [(3,3),(2,2),(2,2),(2,1)])

ritualChamber = [ "###+#+###"
                , "#       #"
                , "#       #"
                , "#       #"
                , "#   *   #"
                , "#       #"
                , "#       #"
                , "#       #"
                , "###+#+###"
                ]

hallway = [ "#################"
          , "#               #"
          , "#   #   #   #   #"
          , "+               +"
          , "#   #   #   #   #"
          , "#               #"
          , "#################"
          ]

levelOne :: LevelSpec
levelOne = LevelSpec
    { generator    = dungeonGen (ritualChamber : hallway : map plainRoom rooms)
    , initializer  = defaultInitializer
    , monsterCount = 0
    , monsterTable = []
    , itemCount    = 40
    , itemTable    = [ (10, (Item.longswordP2, 1))
                     , (10, (Item.greenPotion, 1))
                     , (10, (Item.redPotion, 1))
                     ]
    }

inventoryList :: [(Item, Int)] -> Inventory
inventoryList items = Inventory (Map.fromList (map (\(item, count) -> (Item.name item, count)) items))

initGame :: Game Action Level ()
initGame = do
    loadLevel levelOne

    actor player != Name "Player"
    actor player != inventoryList [(Item.longswordP2, 1), (Item.redPotion, 32)]

    game

game :: Game Action Level ()
game = do
    (AIMod aiM) <- modified player
    noTurn . aiM . handleAct =<< turn

    modifyLevel shadowCast
    
    playerDMap ~= mkDijkstraMap <$> (pure <$> access (loc <# aref player)) <*> access solid

    runEffects

    refs <- monsters <$> level

    forM_ refs $ \ref -> do
        (Hurt amount) <- modified ref
        hp <- getL baseHP <$> modified ref
        monster <- access (aref ref)
        if (hp - amount) <= 0 || dead monster
            then aref ref %= kill
            else runAI ref
      
    game

handleAct :: Action -> Game () Level ()
handleAct (Move dir) = bump player dir
    
handleAct Skip = return ()

handleAct (Activate dir) = do
    pLoc <- access (loc <# aref player)
    player `activate` move4 dir pLoc

handleAct (Cast sname l) =
    case Spell.target (Map.findWithDefault Spell.fizzle sname Spell.every) of
        Spell.None eff -> cast eff
        Spell.Location eff -> cast (eff l)
        Spell.Actor eff -> do
            occupier <- listToMaybe . living (\a -> a ^. loc == l) <$> level
            case occupier of
                Just mon -> cast (eff mon)
                Nothing -> cast Spell.fizzleEffect

handleAct (Pickup l) =
    access (floorItem l) >>= \case
        Nothing -> return ()
        Just item -> do
            pLoc <- access (loc <# aref player)
            (Telekinesis distance) <- modified player
            if manhattan l pLoc > distance
                then message "Item is too far" >> return ()
                else floorItem l != Nothing >> actor player %= addItem item
