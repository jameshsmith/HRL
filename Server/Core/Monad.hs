{-# LANGUAGE TypeOperators, ViewPatterns #-}

-- | In this module, we define the Monad in which game actions occur.
module Core.Monad
    ( -- * The Game monad
      Game, runGame
    , Result (..)
    -- * Dice and random numbers
    , roll, coin, d4, d6, d8, d10, d12, d20, d24, d30, d100
    , Dice (Roll, Const), dice, pick, pickNonEmpty
    -- * Other suspensions
    , yesNo, turn, uniqueInt
    , generate
    , noTurn, rollMax, rollMin
    -- * State functions
    , (!=), (!!=), (%=), (%%=), (~=), (~~=), (#=), (##=)
    , access, with, rd, level, modifyLevel
    , Table, rollTable
    ) where

import Core.Types

import Prelude hiding ((.), id)

import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed (UArray)
import qualified Data.Lens.Lazy as Lens
import System.Random

-- TODO: Re-write with Control.Monad.Free

-- | The 'Result' functor represents the ways in which the game monad can suspend itself. It can:
--
-- * End the turn requesting a new action from the player, represented by the abstract type @k@.
--
-- * Request the caller provide a globally unique integer, which has
-- never been seen before by the Game action itself. It's up to the
-- caller to guarantee uniqueness.
--
-- * Request the caller provide a (user supplied) yes/no answer.
--
-- * Request a random number within a specified range from the
-- caller. It's up to the caller to provide the random number.
--
-- * Request a world generation function be executed.
-- 
-- * Terminate with a result of type @a@.
data Result k l a =
    Turn (k -> Game k l a)
  | Unique (Int -> Game k l a)
  | YesNo (Bool -> Game k l a)
  | Dice (Int, Int) (Int -> Game k l a)
  | Gen (State StdGen (UArray (Row, Col) Char)) (UArray (Row, Col) Char -> Game k l a)
  | Result a

instance Functor (Result k l) where
  fmap f (Turn cont)   = Turn (fmap f . cont)
  fmap f (Unique cont) = Unique (fmap f . cont)
  fmap f (YesNo cont)  = YesNo (fmap f . cont)
  fmap f (Dice n cont) = Dice n (fmap f . cont)
  fmap f (Gen g cont)  = Gen g (fmap f . cont)
  fmap f (Result x)    = Result (f x)

-- | The 'Game' monad is a coroutine/free monad with some additional
-- state @l@ (@l@ stands for level). Think of it as a stateful
-- computation that can suspend itself with a continuation, and can be
-- resumed by its caller later. This suspension is represented by the
-- 'Result' functor.
newtype Game k l a =
  Game { runGame :: State l (Result k l a) }

instance Functor (Game k l) where
  fmap f (Game m) = Game (fmap (fmap f) m)

greturn :: a -> Game k l a
greturn = Game . pure . Result

gbind :: Game k l a -> (a -> Game k l b) -> Game k l b
gbind m f = Game $ do
    res <- runGame m
    case res of
        Turn cont   -> return $ Turn (\act -> cont act `gbind` f)
        Unique cont -> return $ Unique (\n -> cont n `gbind` f)
        YesNo cont  -> return $ YesNo (\b -> cont b `gbind` f)
        Dice n cont -> return $ Dice n (\r -> cont r `gbind` f)
        Gen g cont  -> return $ Gen g (\l -> cont l `gbind` f )
        Result x    -> runGame (f x)

instance Applicative (Game k l) where
  pure = greturn

  m1 <*> m2 = gbind m1 (\f -> gbind m2 (greturn . f))

instance Monad (Game k l) where
  (>>=) = gbind

  return = greturn

-- | 'noTurn' forbids the turn suspension from occuring in its
-- argument, by ignoring it and always returning @()@ instead. This is
-- useful when we want to guarantee that a game action does not
-- request any input from the player - for example during AI routines
-- and suchlike.
noTurn :: Game () l a -> Game k l a
noTurn m = Game $ do
    res <- runGame m
    case res of
        Turn cont   -> runGame $ noTurn (cont ())
        Unique cont -> return $ Unique (noTurn . cont)
        YesNo cont  -> return $ YesNo (noTurn . cont)
        Dice n cont -> return $ Dice n (noTurn . cont)
        Gen g cont  -> return $ Gen g (noTurn . cont)
        Result x    -> return $ Result x

rollMinMax :: ((Int, Int) -> Int) -> Game k l a -> Game k l a
rollMinMax f m = Game $ do
    res <- runGame m
    case res of
        Turn cont   -> return $ Turn (rollMinMax f . cont)
        Unique cont -> return $ Unique (rollMinMax f . cont)
        YesNo cont  -> return $ YesNo (rollMinMax f . cont)
        Dice n cont -> runGame $ rollMinMax f (cont (f n))
        Gen g cont  -> return $ Gen g (rollMinMax f . cont)
        Result x    -> return $ Result x

rollMax :: Game k l a -> Game k l a
rollMax = rollMinMax snd

rollMin :: Game k l a -> Game k l a
rollMin = rollMinMax fst

d4, d6, d8, d10, d12, d20, d24, d30, d100 :: (Int, Int)

-- | Four sided dice
d4 = (1,4)
-- | Six sided dice
d6 = (1,6)
-- | Eight sided dice
d8 = (1,8)
-- | Note that this is a ten sided dice labeled 1-10 rather than the
-- more usual 0-9, so it's consistent with the other polyhedral dice.
d10 = (1,10)
-- | Twelve sided dice
d12 = (1,12)
-- | Twenty sided dice
d20 = (1,20)
-- | Twenty four sided dice
d24 = (1,20)
-- | Thirty sided dice
d30 = (1,30)
-- | Hundred sided dice. Again note that this rolls in the range 1-100
-- rather than 0-99.
d100 = (1,100)

-- | Get a random number between two values inclusive.
roll :: (Int, Int) -> Game k l Int
roll d = Game . return $ Dice d return

data Dice = Roll Int (Int, Int)
          | Plus Dice Dice
          | Mult Dice Dice
          | Negate Dice
          | Const Int
          | Abs Dice
          | Signum Dice

instance Show Dice where
  show (Roll n (lo, hi))
    | lo == hi  = show (n * lo)
    | lo == 1   = show n ++ "d" ++ show hi
    | lo >  1   = show n ++ "d" ++ show (hi + 1 - lo) ++ " + " ++ show (abs (1 - lo))
    | otherwise = show n ++ "d" ++ show (hi + 1 - lo) ++ " - " ++ show (abs (1 - lo))
  show (Plus d1 d2) = show d1 ++ " + " ++ show d2
  show (Mult d1 d2) = show d1 ++ " * " ++ show d2 
  show (Negate d) = "-" ++ show d
  show (Const n) = show n
  show (Abs d) = "abs(" ++ show d ++ ")"
  show (Signum d) = "signum(" ++ show d ++ ")"

instance Num Dice where
  (*)         = Mult
  (+)         = Plus
  negate      = Negate
  fromInteger = Const . fromInteger
  abs         = Abs
  signum      = Signum

dice :: Dice -> Game k l Int
dice (Roll n d)   = fmap sum (replicateM n (roll d))
dice (Plus d1 d2) = (+) <$> dice d1 <*> dice d2
dice (Mult d1 d2) = (*) <$> dice d1 <*> dice d2
dice (Negate d)   = negate <$> dice d
dice (Const d)    = pure d
dice (Abs d)      = abs <$> dice d
dice (Signum d)   = signum <$> dice d

instance Monoid Dice where
  mempty = Const 0

  mappend = Plus

-- | Returns either true or false with 50% probability.
coin :: Game k l Bool
coin = (==) 1 <$> roll (0, 1)

-- | Ask the player a yes/no question.
yesNo :: Game k l Bool
yesNo = Game . return $ YesNo return

-- | End the turn, and return a new action.
turn :: Game k l k
turn = Game . return $ Turn return

-- | Generate a level
generate :: State StdGen (UArray (Row, Col) Char) -> Game k l (UArray (Row, Col) Char)
generate g = Game . return $ Gen g return

-- | Get a globally unique integer.
uniqueInt :: Game k l Int
uniqueInt = Game . return $ Unique return

liftState :: State l a -> Game k l a
liftState = Game . fmap Result

infixr 3 !=, !!=, %=, %%=, ~=, ~~=

-- | Set the value of some lens into the Game state.
(!=) :: l :~> a -> a -> Game k l ()
(weak -> l) != v = void (liftState (l Lens.!= v))

(!!=) :: l :~> a -> a -> Game k l a
(weak -> l) !!= v = liftState (l Lens.!= v) 

-- | Modify the value of some lens into the Game state.
(%=) :: l :~> a -> (a -> a) -> Game k l ()
(weak -> l) %= f = void (liftState (l Lens.%= f))

(%%=) :: l :~> a -> (a -> a) -> Game k l a
(weak -> l) %%= f = liftState (l Lens.%= f)

modifyLevel :: (l -> l) -> Game k l ()
modifyLevel f = liftState (modify f)

-- | Set the value of some lens into the Game state, using a game
-- action.
(~=) :: l :~> a -> Game k l a -> Game k l ()
l ~= m = do { v <- m; l != v }

(~~=) :: l :~> a -> Game k l a -> Game k l a
l ~~= m = do { v <- m; l !!= v}

(#=) :: l :~> a -> (a -> Game k l a) -> Game k l ()
l #= m = do { v <- access l; l ~= m v }

(##=) :: l :~> a -> (a -> Game k l a) -> Game k l a
l ##= m = do { v <- access l; l ~~= m v }

-- | Access some part of the game state.
access :: l :~> a -> Game k l a
access = liftState . Lens.access . weak

level :: Game k l l
level = liftState (Lens.access id)

-- | Perform a game action with some part of the game state.
with :: l :~> a -> (a -> Game k l b) -> Game k l b
with l f = f =<< access l

-- | Read part of the state using an accessor function.
rd :: (l -> a) -> Game k l a
rd f = f <$> (liftState . Lens.access) id

pick :: a -> [a] -> Game k l a
pick x xs@(length -> l) = ((x:xs) !!) <$> roll (0, l)

pickNonEmpty :: [a] -> Game k l a
pickNonEmpty xs@(length -> l) = (xs !!) <$> roll (0, l - 1)

type Table a = [(Int, a)]

rollTable :: Table a -> Game k l a
rollTable tbl = do
    n <- roll (1, sum (map fst tbl))
    return $ fromTable tbl n

fromTable :: Table a -> Int -> a
fromTable [] _ = error "Empty table"
fromTable ((n, x) : tbl') m
  | m <= n    = x
  | otherwise = fromTable tbl' (m - n) 
