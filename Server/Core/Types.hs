{-# LANGUAGE TypeOperators, GADTs #-}

-- | Some basic type definitions and re-exporting universally used
-- libraries.
module Core.Types
  ( Row, Col
  , (:->)
  , (:~>), unsafeWeakLens, weak, (<#)
  , Dir4 (..)
  , Dir8 (..)
  , move4, opposite4, manhattan
  , (?)
  , ix, ixD
  -- * Functor fixpoints and anamorphisms
  , Fix (..)
  , step, eternal, fixF
  -- * Topped Types
  , Topped (..)
  -- * Standard Terminal Colors
  , Color (..)
  -- * Re-exports
  , module Control.Category
  , module Data.Lens.Common
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Data.Array.IArray
import Data.Lens.Common
import qualified Data.Serialize as S

import Test.QuickCheck

-- | A row in an array. Used because @('Row', 'Col')@ is more readable than @(Int, Int)@.
type Row = Int

-- | A column in an array. Used because @('Row', 'Col')@ is more readable than @(Int, Int)@.
type Col = Int

-- | fclabels style type operator for data-lens
type a :-> b = Lens a b

-- | Weak Lenses: Weak lenses @(a :~> b)@ are a subset of lenses that
-- only form a semigroupoid, and not a category. By design there is no
-- id lens, so one cannot access the entire state.
--
-- The intent is to forbid the following (bad) situation:
-- 
-- > action = do
-- >     level <- access id
-- >     monsterRef <- spawn monster
-- >     id != level
-- >     kill monsterRef -- monsterRef no longer exists!
--
-- Notice that by accessing the entire state (level), we can
-- essentially /time travel/ by resetting the state back to a previous
-- state after getting references pointing to the current state!
--
-- While weak lenses do form a semigroupoid, we don't define an
-- instance because all the weak leneses have type @Level :~> a@ where
-- a is not a @Level@. Instead, we use @(<#)@ to combine Weak lenses
-- with ordinary lenses.
--
-- Note that (currently) nothing game-crashingly bad happends if you
-- try to use a undefined reference, and the above code could even
-- make sense as part of a time-travel game mechanic. One could set
-- all undefined monsters as some kind of Dr Who-esque temporal
-- paradox monster, and then trying to access an undefined monster
-- would create such a paradox monster.
newtype a :~> b = WL { weak :: a :-> b } 

-- | Introduce weak lenses from ordinary lenses. It's marked unsafe
-- because @unsafeWeakLens id@ would make @(:~>)@ a category, which
-- breaks the design constract for weak lenses.
unsafeWeakLens :: a :-> b -> a :~> b
unsafeWeakLens = WL

infixr 8 <#

-- | Weak lenses can be combined with ordinary lenses using '(<#)'. It
-- satisfies the law:
-- 
-- > f <# g <# h = (f . g) <# h
--
(<#) :: (b :-> c) -> (a :~> b) -> (a :~> c)
f <# (WL g) = WL (f . g)

-- | Eight way directions
--
-- Abyss is built on 4 way movement, so we don't use these much except
-- in @Core.FOV@.
data Dir8 = N8 | NE8 | E8 | SE8 | S8 | SW8 | W8 | NW8 deriving (Eq, Enum, Bounded, Show)

instance Arbitrary Dir8 where
  arbitrary = arbitraryBoundedEnum

instance S.Serialize Dir8 where
  put d = S.put (fromEnum d)
  get = toEnum <$> S.get

-- | Four way directions
data Dir4 = N4 | E4 | S4 | W4 deriving (Eq, Enum, Show)

instance S.Serialize Dir4 where
  put N4 = S.putWord8 0x00
  put E4 = S.putWord8 0x01
  put S4 = S.putWord8 0x02
  put W4 = S.putWord8 0x03
  
  get = (toEnum . fromIntegral) <$> S.getWord8

-- | Move a @('Row', 'Col')@ pair one unit in a specified direction.
move4 :: Dir4 -> (Row, Col) -> (Row, Col)
move4 N4 (r, c) = (r - 1, c)
move4 E4 (r, c) = (r, c + 1)
move4 S4 (r, c) = (r + 1, c)
move4 W4 (r, c) = (r, c - 1)

-- | Return the opposite compass facing direction.
opposite4 :: Dir4 -> Dir4
opposite4 N4 = S4
opposite4 E4 = W4
opposite4 S4 = N4
opposite4 W4 = E4

-- | The manhattan (taxicab) distance between two locations.
manhattan :: (Row, Col) -> (Row, Col) -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

(?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
arr ? i | bounds arr `inRange` i = Just (arr ! i)
        | otherwise = Nothing

ix :: (IArray a e, Ix i) => i -> a i e :-> e
ix i = lens (! i) (\v arr -> arr // [(i, v)]) 

ixD :: (IArray a e, Ix i) => e -> a i e -> i -> e
ixD d arr i | bounds arr `inRange` i = arr ! i
            | otherwise = d

-- | A @Fix f@ is a function @s -> f s@ with some state @s@. Notice
-- that we use GADTs/existential types to hide the state @s@. The
-- state is therefore /encapsulated/ within the type. They are mostly
-- used implement transition systems for things like AI routines and
-- effects occuring over time, where the internal state is used to
-- represent either the state in the transition system, or the
-- progress of the effect respectively.
--
-- More abstractly we can think of @Fix f@ as a fixpoint of the
-- functor @f@ given by its anamorphism. I suspect (but have not
-- proved) that this is equivalent to the traditional definition:
--
-- > data FixA f = Fix { unFix :: (f (FixA f)) }
--
-- as we can define:
--
-- > ana :: Functor f => (a -> f a) -> (a -> FixA f)
-- > ana f = Fix . fmap (ana f) . f
-- >
-- > data FixB f where
-- >   Ana :: (s -> f s) -> s -> FixB f
-- >
-- > bToA :: Functor f => FixB f -> FixA f
-- > bToA (Ana f x) = ana f x
-- >
-- > aToB :: FixA f -> FixB f 
-- > aToB f = Ana unFix f
data Fix f where
  Ana :: (s -> f s) -> s -> Fix f

instance Applicative f => Monoid (Fix f) where
  mempty = Ana pure ()

  mappend (Ana f x) (Ana g y) = Ana h (x, y)
    where
      h (a, b) = (,) <$> f a <*> g b

step :: Functor f => Fix f -> f (Fix f)
step (Ana f x) = Ana f <$> f x

fixF :: Functor f => f (Fix f) -> Fix f
fixF = Ana (fmap step)

eternal :: Functor f => f () -> Fix f
eternal g = Ana (const g) ()

-- | @Topped a@ is simply the type @a@ with a designated top element
-- @Top@ with the @Bounded@ and @Ord@ typeclasses defined
-- appropriately.
data Topped a = Top | NotTop !a deriving (Eq, Show)

instance Bounded a => Bounded (Topped a) where
  minBound = NotTop minBound
  maxBound = Top

instance Ord a => Ord (Topped a) where
  _ <= Top = True
  Top <= _ = False
  (NotTop x) <= (NotTop y) = x <= y

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
