{-# LANGUAGE TupleSections, TypeOperators, ViewPatterns #-}
module Core.Engine
    ( ARef
    , Level
    , Status
    , Actor
    , Spell (..)
    , Effect (..)
    , hidden, alive, dead, kill, resurrect
    , spawn
    , player
    , loc
    , aref
    , actors
    , living
    , monsters
    , corpses
    , actor, actorChar
    , playerDMap
    , opacity, visible, shadowCast, seen, solid
    , statics, static, staticChar
    , message, messageLog, clearMessages
    , runEffects
    , cast
    , defaultLevel
    , levelToJSON
    ) where

import Prelude hiding ((.), id)

import Core.DijkstraMap
import Core.ECS (Entity, Component, HasEntity)
import qualified Core.ECS as ECS
import qualified Core.FOV as FOV
import Core.Monad
import Core.Types

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Array.Unboxed
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Vector as V

newtype ARef = ARef Int deriving (Eq, Ord, Show)

data Level = Level
    { _opacity    :: UArray (Row, Col) Bool
    , _visible    :: UArray (Row, Col) Bool
      -- | @_seen@ needs to be strict or we end up building up a huge
      -- (hundreds of megabytes) thunk while loading levels.
    , _seen       :: !(UArray (Row, Col) Bool)
    , _solid      :: UArray (Row, Col) Bool
    , _player     :: Int
    , _egg        :: Actor
    , _actors     :: IntMap Actor
    , _effects    :: IntMap Effect
    , _statics    :: Array (Row, Col) (Char, Entity)
    , _playerDMap :: DijkstraMap
    , _messageLog :: [String]
    }

data Status = Alive | Dead | Hidden deriving (Eq, Show)

data Actor = Actor
    { _eloc    :: (Row, Col)
    , _status  :: Status
    , _achar   :: Char
    , _aentity :: Entity
    }

instance HasEntity Actor where
  entity = lens _aentity (\v l -> l { _aentity = v })

spawn :: (Row, Col) -> (Char, Entity) -> Game k Level ARef
spawn pos (chr, ent) = do
    uid <- uniqueInt
    modifyLevel (\l -> l { _actors = IMap.insert uid (Actor pos Alive chr ent) (_actors l) })
    return (ARef uid)

hidden :: Actor -> Bool
hidden = (==) Hidden . _status

alive :: Actor -> Bool
alive = (==) Alive . _status

dead :: Actor -> Bool
dead = (==) Dead . _status

kill :: Actor -> Actor
kill e = if _status e == Alive then e { _status = Dead } else e

resurrect :: Actor -> Actor
resurrect e = if _status e == Dead then e { _status = Alive } else e

-- | Lens for an actors location
loc :: Actor :-> (Row, Col)
loc = lens _eloc (\v e -> e { _eloc = v })

-- | Actor reference for the player.
player :: ARef
player = ARef 0

-- | A list of all (non-hidden) actors satisfying some predicate.
actors :: (Actor -> Bool) -> Level -> [ARef]
actors p = map (ARef . fst) . filter predicate . IMap.assocs . _actors
  where
    predicate e = not (hidden (snd e)) && p (snd e)

-- | A list of all living actors satisfying some predicate.
living :: (Actor -> Bool) -> Level -> [ARef]
living p = map (ARef . fst) . filter predicate . IMap.assocs . _actors
  where
    predicate (_, e) = p e && alive e

-- | Get an actor lens from an actor reference. 
aref :: ARef -> Level :~> Actor
aref (ARef k) = unsafeWeakLens (lens get set)
  where
    get l = maybe (_egg l) id . IMap.lookup k $ _actors l

    set v l = l { _actors = IMap.insert k v (_actors l) }

-- | Actor references for all the monsters in the level, where
-- monsters are actors that are alive and not the player.
monsters :: Level -> [ARef]
monsters l = map (ARef . fst) . filter predicate . IMap.assocs $ _actors l
  where predicate (ref, e) = alive e && ARef ref /= player

-- | Actor references for all the corpses (dead actors) in the level
corpses :: Level -> [ARef]
corpses l = map (ARef . fst) . filter predicate . IMap.assocs $ _actors l
  where predicate (ref, e) = dead e && ARef ref /= player

-- | Lens to access components using some actor reference.
actor :: Component s => ARef -> Level :~> s
actor ref = ECS.lens . lens _aentity (\v l -> l { _aentity = v }) <# aref ref

actorChar :: ARef -> Level :~> Char
actorChar ref = lens _achar (\v l -> l { _achar = v }) <# aref ref

-- | Identifies which cells can be seen through for the purposes of
-- calculating FOV. Usually the same as 'solid'.
opacity :: Level :~> UArray (Row, Col) Bool
opacity = unsafeWeakLens (lens _opacity (\v l -> l { _opacity = v }))

-- | A Dijkstra map that allows monsters to find the player. Must be
-- manually updated whenever the player moves.
playerDMap :: Level :~> DijkstraMap
playerDMap = unsafeWeakLens (lens _playerDMap (\v l -> l { _playerDMap = v }))

-- | Get the array containing currently visible cells. Must be updated
-- with 'shadowCast'.
visible :: Level -> UArray (Row, Col) Bool
visible = _visible

-- | Set the visible cells to those that the player can see, and
-- update the seen cells.
shadowCast :: Level -> Level
shadowCast l = l { _visible = vis, _seen = FOV.updateSeen vis (_seen l) }
  where
    ploc = loc . weak (aref player) ^$ l
    vis = FOV.shadowCast ploc (_opacity l)

-- | Lens for the cells the player has seen.
seen :: Level :~> UArray (Row, Col) Bool
seen = unsafeWeakLens (lens _seen (\v l -> l { _seen = v }))

-- | Used to identify which cells can be traversed.
solid :: Level :~> UArray (Row, Col) Bool
solid = unsafeWeakLens (lens _solid (\v l -> l { _solid = v }))

statics :: Level :~> Array (Row, Col) (Char, Entity)
statics = unsafeWeakLens (lens _statics (\v l -> l { _statics = v }))

static :: Component s => (Row, Col) -> Level :~> s
static pos = ECS.lens . sndLens . ix pos <# statics

staticChar :: (Row, Col) -> Level :~> Char
staticChar pos = fstLens . ix pos <# statics

-- FIXME: This was causing a big space leak...
message :: String -> Game k Level ()
message msg = modifyLevel (\l -> l { _messageLog = msg : _messageLog l })

clearMessages :: Level -> Level
clearMessages l = l { _messageLog = [] }

messageLog :: Level -> [String]
messageLog = _messageLog

data Spell =
      TargetActor (ARef -> Effect)
    | TargetLocation ((Row, Col) -> Effect)
    | TargetNone Effect

data Effect = Effect
    { effectDispel :: Maybe (Game () Level ())
    , effectAction :: Fix (MaybeT (Game () Level))
    }

cast :: Effect -> Game k Level ()
cast eff = do
    uid <- uniqueInt
    modifyLevel (\l -> l { _effects = IMap.insert uid eff (_effects l) })

effects :: Level :~> IntMap Effect
effects = unsafeWeakLens (lens _effects (\v l -> l { _effects = v }))

runEffects :: Game k Level ()
runEffects = noTurn $ do
    effs <- access effects
    forM_ (IMap.keys effs) $ \key -> do
        let (Just (Effect d act)) = IMap.lookup key effs
        res <- runMaybeT (step act)
        case res of
            Nothing   -> effects %= IMap.delete key
            Just act' -> effects %= IMap.insert key (Effect d act')

defaultFloor' :: UArray (Row, Col) Char
defaultFloor' = listArray ((0,0), (length dun - 1, length (head dun) - 1)) (concat dun)
  where
    dun = [ "###"
          , "# #"
          , "###"
          ]

defaultFloor :: UArray (Row, Col) Bool
defaultFloor = amap (== '#') defaultFloor'

defaultLevel :: Level
defaultLevel = Level
    { _opacity    = defaultFloor
    , _visible    = FOV.shadowCast (1,1) defaultFloor
    , _seen       = FOV.shadowCast (1,1) defaultFloor
    , _solid      = defaultFloor
    , _egg        = Actor (1,1) Hidden 'e' ECS.empty
    , _player     = 0
    , _actors     = IMap.insert 0 (Actor (1,1) Alive '@' ECS.empty) IMap.empty
    , _effects    = IMap.empty
    , _statics    = array (bounds defaultFloor) (map (second (, ECS.empty)) (assocs defaultFloor'))
    , _playerDMap = mkDijkstraMap [(1,1)] defaultFloor
    , _messageLog = []
    }

-- | Serialize levels
arrayToJSON :: IArray a e => (e -> Char) -> a (Row, Col) e -> J.Value
arrayToJSON toChar arr@(bounds -> (_, (mr, mc))) = J.Array $ V.generate (mr + 1) textRow
  where
    textRow r = J.String . T.pack $ map (toChar . (!) arr . (,) r) [0..mc]

actorToJSON entityToJSON actor = J.object
  [ (T.pack "row", J.toJSON . fst $ _eloc actor)
  , (T.pack "col", J.toJSON . snd $ _eloc actor)
  , (T.pack "chr", if _status actor == Dead then J.toJSON 'c' else J.toJSON (_achar actor))
  , (T.pack "entity", entityToJSON (_aentity actor))
  ]

actorsToJSON :: (Entity -> J.Value) -> IntMap Actor -> J.Value
actorsToJSON entityToJSON = J.object . IMap.foldrWithKey f []
  where
    f k v acc = (T.pack ('a' : show k), actorToJSON entityToJSON v) : acc

levelToJSON :: (Entity -> J.Value) -> Level -> J.Value
levelToJSON entityToJSON lev = J.object
    [ (T.pack "rows", J.toJSON . succ . fst . snd . bounds $ _statics lev)
    , (T.pack "cols", J.toJSON . succ . snd . snd . bounds $ _statics lev)
    , (T.pack "statics", arrayToJSON fst (_statics lev))
    , (T.pack "visible", arrayToJSON boolChar (_visible lev))
    , (T.pack "seen", arrayToJSON boolChar (_seen lev))
    , (T.pack "actors", actorsToJSON entityToJSON (_actors lev))
    , (T.pack "messages", J.toJSON (_messageLog lev))
    ]
  where
    boolChar True = '#'
    boolChar False = ' '

