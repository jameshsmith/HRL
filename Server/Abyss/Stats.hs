{-# LANGUAGE OverloadedStrings, TypeOperators, MultiWayIf, GeneralizedNewtypeDeriving #-}
module Abyss.Stats where

import Prelude hiding ((.), id)

import Core.Types
import qualified Core.ECS as ECS
import Core.Monad
import Core.Engine
import Component.Name
import Component.Modifier

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

data DamageType =
    Slashing
  | Bashing
  | Piercing
  | Fire
  | Ice
  | Electrical
  | Acid
  | Magical
  | Necrotic
  | Radiant
  deriving (Eq, Ord, Show)
    
-- | The 'Stats' record contains all the information on a characters
-- attributes and abilities.
data Stats = Stats
    { _str :: (Int, Topped Int)
    , _dex :: (Int, Topped Int)
    , _con :: (Int, Topped Int)
    , _int :: (Int, Topped Int)
    , _pow :: (Int, Topped Int)
    , _attack :: Int
    , _defense :: Int
    , _skills :: Map Text Int
    , _resists :: Map DamageType Int
    , _baseHP :: Int
    , _baseMana :: Int
    , _damage :: [(Dice, DamageType)]
    } deriving (Show)

-- | The Attributes of Abyss are:
--
-- * @STR@ - The strength and physical power of a character.
-- * @DEX@ - The speed and physical dexterity of a character.
-- * @CON@ - The constitution and physical toughness of a character.
-- * @INT@ - How intelligent the character is.
-- * @POW@ - The raw magical power of a character.
--
-- Thought... if there's five attributes, maybe it's fun to put them
-- on a pentagram?
--
-- >         _POW__
-- >     .-'`  /\  `'-.
-- >INT.'_____/__\_____`.CON
-- >  / `._  /    \  _.` \
-- > |     `/      \'     |
-- > |     / `-..-' \     |
-- >  \   /  .-'`-.  \   /
-- >   `./.-'      `-.\.'
-- >  DEX`-..______..-'STR
--
data Attribute = STR | DEX | CON | INT | POW deriving (Eq, Show)

-- | Lenses to access an attribute from the Stat object.
attribute :: Attribute -> Stats :-> Int
attribute = (fstLens .) . attribute'

-- | The max bonus for an attribute.
maxBonus :: Attribute -> Stats :-> Topped Int
maxBonus = (sndLens .) . attribute'

-- Access an attribute and its maximum bonus.
attribute' :: Attribute -> Stats :-> (Int, Topped Int)
attribute' STR = lens _str (\v s -> s { _str = v })
attribute' DEX = lens _dex (\v s -> s { _dex = v })
attribute' CON = lens _con (\v s -> s { _con = v })
attribute' INT = lens _int (\v s -> s { _int = v })
attribute' POW = lens _pow (\v s -> s { _pow = v })

damage :: Stats :-> [(Dice, DamageType)]
damage = lens _damage (\v s -> s { _damage = v })

totalAttributes :: Stats -> Int
totalAttributes stats = sum $ map (\a -> stats ^. attribute a) [STR, DEX, CON, INT, POW]

-- | Return the bonus for an attribute.
bonus :: Attribute -> Stats -> Int
bonus a stats
  | (NotTop mb) <- stats ^. maxBonus a = b `min` mb
  | otherwise = b
  where
    b = (stats ^. attribute a - 10) `div` 2

attack :: Stats :-> Int
attack = lens _attack (\v s -> s { _attack = v })

defense :: Stats :-> Int
defense = lens _defense (\v s -> s { _defense = v })

baseHP :: Stats :-> Int
baseHP = lens _baseHP (\v s -> s { _baseHP = v })

defaultStats :: Stats
defaultStats = Stats attr attr attr attr attr 0 10 Map.empty Map.empty 10 10 [(Roll 1 d6, Bashing)]
  where
    attr = (10, Top)

instance ECS.Component Stats where
  stock = defaultStats

data RollResult = Botch | Fail | Pass | Critical deriving (Eq, Show)

rollToHit :: Int -> Stats -> Stats -> Game k l ((Int, Int), RollResult)
rollToHit threat attacker defender = do
    n <- roll d20
    return . (,) (n, def - atk) $ if
      | n == 1         -> Botch
      | n >= threat    -> Critical
      | n + atk >= def -> Pass
      | otherwise      -> Fail
  where
    atk = attacker ^. attack + bonus STR attacker

    def = defender ^. defense + bonus DEX defender

rollDamage :: Stats -> Stats -> Game k l [(Int, DamageType)]
rollDamage attacker defender = do
    fmap absorb $ forM (_damage attacker) $ \(amount, dtype) ->
        fmap (\x -> (x - Map.findWithDefault 0 dtype (_resists defender), dtype)) (dice amount)
  where
    absorb = filter ((> 0) . fst) . map (\(amount, dtype) -> (amount - bonus CON defender, dtype))

meleeAttack :: ARef -> ARef -> Game k Level ()
meleeAttack attacker defender = do
    astats <- modified attacker
    dstats <- modified defender
  
    ((n, dc), res) <- rollToHit 20 astats dstats

    (Name aName) <- modified attacker
    (Name dName) <- modified defender
    
    message (T.concat [ aName, " attacks ", dName
                      , " (roll:", T.pack (show n)
                      , " DC:", T.pack (show dc)
                      , " *", T.pack (show res), "*)"
                      ])
    
    case res of
      Botch    -> return ()  -- TODO: Think of something mean
      Fail     -> return ()
      Pass     -> do
          dam <- rollDamage astats dstats
          actor defender %= (+ Hurt (sum (map fst dam)))
          message ("did " <> T.intercalate ", " (map prettyDam dam))

      Critical -> do
          dam <- rollMax (rollDamage astats dstats)
          actor defender %= (+ Hurt (sum (map fst dam)))
          message ("did " <> T.intercalate ", " (map prettyDam dam) <> "!")
  where
    prettyDam (n, d) = T.pack (show n) <> " " <> T.toLower (T.pack (show d))
    
mergeStats :: (Int -> Int -> Int) -> Stats -> Stats -> Stats
mergeStats f s1 s2 = Stats
    { _str = _str s1 `attrPlus` _str s2
    , _dex = _dex s1 `attrPlus` _dex s2
    , _con = _con s1 `attrPlus` _con s2
    , _int = _int s1 `attrPlus` _int s2
    , _pow = _pow s1 `attrPlus` _pow s2
    , _attack = _attack s1 `f` _attack s2
    , _defense = _defense s1 `f` _defense s2
    , _skills = Map.unionWith f (_skills s1) (_skills s2)
    , _resists = Map.unionWith f (_resists s1) (_resists s2)
    , _baseHP = _baseHP s1 `f` _baseHP s2
    , _baseMana = _baseMana s1 `f` _baseMana s2
    , _damage = _damage s1 ++ _damage s2
    }
  where
    attrPlus (x, mx) (y, my) = (f x y, mx `min` my)

instance Monoid Stats where
  mempty = Stats attr attr attr attr attr 0 0 Map.empty Map.empty 0 0 []
    where
      attr = (0, Top)

  mappend = mergeStats (+)

newtype Hurt = Hurt { hurt :: Int } deriving (Eq, Ord, Num)

instance ECS.Component Hurt where
  stock = Hurt 0
