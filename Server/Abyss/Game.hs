{-# LANGUAGE LambdaCase #-}
module Abyss.Game (initGame, Action (..)) where

import Prelude hiding ((.), id)

import Abyss.Stats
import qualified Abyss.Spell as Spell
import Core.Types
import Core.Engine
import Core.Monad
import Core.DijkstraMap
import qualified Core.ECS as ECS
import Component.Activator
import Component.AI
import Component.Name
import Component.Modifier
import Gen.Dungeon

import Control.Arrow (second)
import Control.Monad
import Data.Array.Unboxed
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Serialize as S
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.Text as T

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

data Action = Move Dir4
            | Skip
            | Activate Dir4
            | Cast String (Row, Col)
            deriving Show

parseDir4 :: MonadPlus m => String -> m Dir4
parseDir4 "N" = pure N4
parseDir4 "E" = pure E4
parseDir4 "S" = pure S4
parseDir4 "W" = pure W4
parseDir4 _ = mzero

parseSkip :: MonadPlus m => T.Text -> m Action
parseSkip t
  | t == T.pack "skip" = return Skip
  | otherwise = mzero

instance J.FromJSON Action where
  parseJSON v = msum
      [ J.withObject "move" (\obj -> fmap Move (parseDir4 =<< obj .: T.pack "move")) v
      , J.withText "skip" parseSkip v
      , J.withObject "activate" (\obj -> fmap Activate (parseDir4 =<< obj .: T.pack "activate")) v
      , flip (J.withObject "cast") v $ \obj -> do
          n <- obj .: T.pack "name"
          r <- obj .: T.pack "row"
          c <- obj .: T.pack "col"
          return (Cast n (r, c))
      ]

instance S.Serialize Action where
  put (Move d)     = S.putWord8 0x00 >> S.put d
  put Skip         = S.putWord8 0x01
  put (Activate d) = S.putWord8 0x02 >> S.put d
  put (Cast s l)   = S.putWord8 0x03 >> S.put s >> S.put l 

  get = S.getWord8 >>= \case
      0x00 -> Move <$> S.get
      0x01 -> return Skip
      0x02 -> Activate <$> S.get
      0x03 -> Cast <$> S.get <*> S.get
      _    -> error "Invalid action!"

rooms :: [(Int, Int)]
rooms = [(6,7),(6,5),(4,4),(3,4),(4,2)] ++ concat (repeat [(3,3),(2,2),(2,2),(2,1)])

initStatic :: Bool -> (Char, ECS.Entity)
initStatic True = ('#', ECS.empty)
initStatic False = (' ', ECS.empty)

initGame :: Game Action Level ()
initGame = do
    layout <- generate (dungeonGen (map plainRoom rooms))

    actor player != Name "Player"

    let s = amap (/= ' ') layout
    solid != s
    opacity != s
    seen != listArray (bounds s) (repeat False)
    statics != array (bounds s) (map (second initStatic) (assocs s))

    let freeSpot = fst . head $ filter ((==) False . snd) (assocs s)

    loc <# aref player != freeSpot
    playerDMap != mkDijkstraMap [freeSpot] s

    modifyLevel shadowCast
    
    forM [1..50] $ \_ -> void $ spawn freeSpot ('Z', zombie)

    let doors = filter ((== '+') . snd) (assocs layout)

    forM_ doors $ \(d, _) -> do
        staticChar d != '+'
        static d != door
        static d != Name "door"
    
    game

game :: Game Action Level ()
game = do
    (AIMod aiM) <- modified player
    noTurn . aiM . handleAct =<< turn

    modifyLevel shadowCast
    
    playerDMap ~= mkDijkstraMap <$> (pure <$> access (loc <# aref player)) <*> access solid

    runEffects

    refs <- monsters <$> level

    mapM_ runAI refs
      
    game

handleAct :: Action -> Game () Level ()
handleAct (Move dir) = bump player dir
    
handleAct Skip = return ()

handleAct (Activate dir) = do
    pLoc <- access (loc <# aref player)
    player `activate` move4 dir pLoc

handleAct (Cast sname l) =
    case Map.findWithDefault Spell.fizzle sname Spell.every of
        TargetNone eff -> cast eff
        TargetLocation eff -> cast (eff l)
        TargetActor eff -> do
            occupier <- listToMaybe . living (\a -> a ^. loc == l) <$> level
            case occupier of
                Just mon -> cast (eff mon)
                Nothing -> cast Spell.fizzleEffect
