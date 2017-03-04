{-# LANGUAGE TypeOperators, ViewPatterns #-}

-- | In this module, we define the Monad in which game actions occur.
module Core.Monad
    ( -- * The Game monad
      Game, runGame, runGameState
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
import Control.Monad.Trans.Free
import Control.Monad.State
import Data.Array.Unboxed (UArray)
import qualified Data.Lens.Lazy as Lens
import System.Random

data Result k a =
    Turn (k -> a)
  | Unique (Int -> a)
  | YesNo (Bool -> a)
  | Dice (Int, Int) (Int -> a)
  | Gen (State StdGen (UArray (Row, Col) Char)) (UArray (Row, Col) Char -> a)

instance Functor (Result k) where
  fmap f (Turn cont)   = Turn (f . cont)
  fmap f (Unique cont) = Unique (f . cont)
  fmap f (YesNo cont)  = YesNo (f . cont)
  fmap f (Dice n cont) = Dice n (f . cont)
  fmap f (Gen g cont)  = Gen g (f . cont)

type Game k l = FreeT (Result k) (State l)

runGame :: Game k l a -> State l (FreeF (Result k) a (Game k l a))
runGame = runFreeT

runGameState :: Game k s a -> s -> (FreeF (Result k) a (Game k s a), s)
runGameState = runState . runGame

noTurn :: Game () l a -> Game k l a
noTurn m = FreeT $ do
    res <- runGame m
    case res of
        Pure x -> return (Pure x)
        Free (Turn cont)   -> runGame (noTurn (cont ()))
        Free (Unique cont) -> return . Free $ Unique (noTurn . cont)
        Free (YesNo cont)  -> return . Free $ YesNo (noTurn . cont)
        Free (Dice n cont) -> return . Free $ Dice n (noTurn . cont)
        Free (Gen g cont)  -> return . Free $ Gen g (noTurn . cont)

rollMinMax :: ((Int, Int) -> Int) -> Game k l a -> Game k l a
rollMinMax f m = FreeT $ do
    res <- runGame m
    case res of
        Pure x -> return (Pure x)
        Free (Turn cont)   -> return . Free $ Turn (rollMinMax f . cont)
        Free (Unique cont) -> return . Free $ Unique (rollMinMax f . cont)
        Free (YesNo cont)  -> return . Free $ YesNo (rollMinMax f . cont)
        Free (Dice n cont) -> runGame $ rollMinMax f (cont (f n))
        Free (Gen g cont)  -> return . Free $ Gen g (rollMinMax f . cont)

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
roll d = liftF $ Dice d id

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
yesNo = liftF $ YesNo id

-- | End the turn, and return a new action.
turn :: Game k l k
turn = liftF $ Turn id

-- | Generate a level
generate :: State StdGen (UArray (Row, Col) Char) -> Game k l (UArray (Row, Col) Char)
generate g = liftF $ Gen g id

-- | Get a globally unique integer.
uniqueInt :: Game k l Int
uniqueInt = liftF $ Unique id

infixr 3 !=, !!=, %=, %%=, ~=, ~~=

-- | Set the value of some lens into the Game state.
(!=) :: l :~> a -> a -> Game k l ()
(weak -> l) != v = void (lift (l Lens.!= v))

(!!=) :: l :~> a -> a -> Game k l a
(weak -> l) !!= v = lift (l Lens.!= v) 

-- | Modify the value of some lens into the Game state.
(%=) :: l :~> a -> (a -> a) -> Game k l ()
(weak -> l) %= f = void (lift (l Lens.%= f))

(%%=) :: l :~> a -> (a -> a) -> Game k l a
(weak -> l) %%= f = lift (l Lens.%= f)

modifyLevel :: (l -> l) -> Game k l ()
modifyLevel f = lift (modify f)

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
access = lift . Lens.access . weak

level :: Game k l l
level = lift (Lens.access id)

-- | Perform a game action with some part of the game state.
with :: l :~> a -> (a -> Game k l b) -> Game k l b
with l f = f =<< access l

-- | Read part of the state using an accessor function.
rd :: (l -> a) -> Game k l a
rd f = f <$> (lift . Lens.access) id

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
fromTable [] n = error ("Empty table: rolled " ++ show n)
fromTable ((n, x) : tbl') m
  | m <= n    = x
  | otherwise = fromTable tbl' (m - n) 
