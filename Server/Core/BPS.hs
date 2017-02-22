{-# LANGUAGE ViewPatterns, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Core.BPS
    ( preProcess
    , pathfind
    ) where

import Prelude hiding ((.), id)

import Core.Types

import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits
import Data.Maybe
import Data.Word

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Heap (Heap, Entry)
import qualified Data.Heap as Heap

dirBit :: Dir4 -> Word16
dirBit N4 = bit 0
dirBit E4 = bit 2
dirBit S4 = bit 4
dirBit W4 = bit 6

-- | the @preProcess@ function pre-processes the level solidity array,
-- which is True when a tile is solid (or non-traversable) and False
-- when a tile is traversable. For each tile it encodes a 16-bit word
-- which encodes the adjacency information for that tile (i.e. whether
-- the tiles to the north, east, south and west are blocked), as well
-- as the /hooks/. The adjacency information is stored in the lowest 8
-- bits, while the hooks are in the upper bit. An example of a such a
-- Word16 would be:
-- 
-- > -----------------------------
-- > |  w  s  e  n |             |
-- > | ac ac ac ac |  w  s  e  n |
-- > | 11 01 00 00 | 01 00 00 00 | = 0xD10 = 3344
-- > -----------------------------
-- > | hooks       | adjacency   |
-- > -----------------------------
--
-- The adjacency here is telling us that for the tile, the tile
-- directly west of it is solid. All the other tiles are free.
-- Calling the tile @x@ we can represent this as
--
-- >        +---+---+---+
-- >        |XXX|   |XXX|
-- >        +---+---+---+
-- >        |XXX|   |   |
-- >        +---+---+---+
-- >        |XXX| n |   |
-- >        +---+---+---+
-- >        |XXX|   |XXX|
-- >        +---+---+---+
--
-- Where XXX represents a solid tile. The hooks tell us when we enter
-- a tile from a direction, do we need to hook a left or a right turn
-- (anticlockwise and clockwise respectively). This is what the @a@
-- and @c@ bits mean for each direction.
--
-- When do we need to do this? Consider moving into this tile from m
-- in the east (going WEST):
--
-- >        +---+---+---+
-- >        |XXX|>=2|XXX|
-- >        +---+---+---+
-- >        |XXX|   | 1 |
-- >        +---+---+---+
-- >        |XXX| n<==m |
-- >        +---+---+---+
-- >        |XXX|   |XXX|
-- >        +---+---+---+
--
-- We check the tiles northwards of n and m. For m the nearest
-- northward wall is 1 away, while for n the nearest wall is at least
-- two or greater away (it's not on the picture). Because 2 is
-- strictly greater than 1, we potentially need to turn clockwise to
-- explore this new space that's opened up to our north. We also need
-- to turn anticlockwise for the same reason, as more space has opened
-- up to our south. Essentially for every direction moving into every
-- square, we check whether the dungeon wall in the perpendicular
-- directions is increasing in distance, and if it is, we need to
-- explore that new space so we /hook/ a turn in that direction. This
-- isn't very useful here, as we will hit a wall and need to turn
-- anyway, but consider the following example:
--
-- >        +---+---+---+
-- >        |XXX| a |XXX|
-- >        +---+---+---+
-- >        |XXX| b | c |
-- >        +---+---+---+
-- >        |XXX| d | e |
-- >        +---+---+---+
-- >        |XXX| f |XXX|
-- >        +---+---+---+
--
-- If we are going from f to d, we must hook a right (clockwise). If
-- we go from b to d we just continue onwards to f without trying to
-- explore e. This is fine, and won't impact our search, because we'll
-- explore e if necessary because we'll hook a left going from a to b,
-- reaching c and then exploring e if neccessary from that direction
-- (should the square east of c be blocked). Essentially we use this
-- fact to exploit the symmetry of a grid map and massively reduce the
-- number of points we need to search compared to a naive A* search.
--
-- The clever bit of the encoding as a Word16 is that we can view as
-- square from a specific direction by bitshifting it by 2 * the
-- direction (where north is 0, east is 1, south is 2, and west is 3)
-- and then masking it with 0x301 (see below). If the result of this
-- is greater than 0 then we've either blocked or reached turning
-- point where we need to hook a left or right turn. This allows us to
-- avoid many expensive comparisons in the core of our pathfinding
-- loop.
--
-- > w `shiftR` (fromEnum d * 2) .&. 0x301
--
-- All my tests show that this algorithm always finds the optimal path
-- if it exists, but I haven't seen it in the literature so I can't
-- say this for sure. Overall what it's trying to do is exploit the
-- grid symmetries to prune the A* search tree, so assuming this
-- pruning is good (and I believe it is) we should be fine.
--
-- I'm calling it BPS or Box Point Search for now, because it's kinda
-- somewhat like JPS but it works with boxes.
preProcess :: UArray (Row, Col) Bool -> UArray (Row, Col) Word16
preProcess solid = runSTUArray $ do
    let (_, (maxR, maxC)) = bounds solid
    arr <- newArray ((0, 0), (maxR, maxC)) 0
    forM_ (range ((0, 0), (maxR, maxC))) $ \p ->
        when (not (solid ! p)) $ writeArray arr p (bitfield solid p)
    return arr

bitfield :: UArray (Row, Col) Bool -> (Row, Col) -> Word16
bitfield s p = adjacency s p .|. foldr (.|.) 0 (map (turnPoint s p) [N4, E4, S4, W4])

adjacency :: UArray (Row, Col) Bool -> (Row, Col) -> Word16
adjacency solid p = test N4 .|. test E4 .|. test S4 .|. test W4
  where
    test d = if solid ! move4 d p then dirBit d else 0

-- | Distance to a wall in a direction.
d2w :: UArray (Row, Col) Bool -> (Row, Col) -> Dir4 -> Int
d2w solid p d
  | solid ! p = 0
  | otherwise = 1 + d2w solid (move4 d p) d

turnPoint :: UArray (Row, Col) Bool -> (Row, Col) -> Dir4 -> Word16
turnPoint solid (r, c) d = jp d `shiftL` (fromEnum d * 2)
  where
    -- Going south
    jp S4 = (d2w solid (r - 1, c - 1) W4) `lt8` (d2w solid (r, c - 1) W4)  -- Hook clockwise
        .|. (d2w solid (r - 1, c + 1) E4) `lt9` (d2w solid (r, c + 1) E4)  -- Hook anticlockwise

    -- Going west
    jp W4 = (d2w solid (r - 1, c + 1) N4) `lt8` (d2w solid (r - 1, c) N4)  -- Hook clockwise
        .|. (d2w solid (r + 1, c + 1) S4) `lt9` (d2w solid (r + 1, c) S4)  -- Hook anticlockwise

    -- Going north
    jp N4 = (d2w solid (r + 1, c + 1) E4) `lt8` (d2w solid (r, c + 1) E4)  -- Hook clockwise
        .|. (d2w solid (r + 1, c - 1) W4) `lt9` (d2w solid (r, c - 1) W4)  -- Hook anticlockwise

    -- Going east
    jp E4 = (d2w solid (r + 1, c - 1) S4) `lt8` (d2w solid (r + 1, c) S4)  -- Hook clockwise
        .|. (d2w solid (r - 1, c - 1) N4) `lt9` (d2w solid (r - 1, c) N4)  -- Hook anticlockwise

lt8 :: Int -> Int -> Word16
lt8 x y | x < y    = bit 8
        | otherwise = 0

lt9 :: Int -> Int -> Word16
lt9 x y | x < y     = bit 9
        | otherwise = 0

-- | d `into` w is nonzero if w is either a left or a right hook, or d
-- is blocked leaving w.
into :: Dir4 -> Word16 -> Word16
into d w = w `shiftR` (fromEnum d * 2) .&. 0x301

openFrom :: Dir4 -> Word16 -> [Dir4]
openFrom from w = filter (\d -> (w .|. dirBit (opposite4 from)) .&. dirBit d == 0) [N4, E4, S4, W4]

open :: Word16 -> [Dir4]
open w = filter (\d -> w .&. dirBit d == 0) [N4, E4, S4, W4]

hook :: Dir4 -> Word16 -> Word16
hook d w = w `shiftR` (fromEnum d * 2 + 8) .&. 0x3

data Path = Path { distance :: Int, dir :: Dir4, nodes :: [(Row, Col)] } deriving Show

intersect :: Dir4 -> (Row, Col) -> (Row, Col) -> Bool
intersect d (r1, c1) (r2, c2)
  | d == N4 || d == S4 = r1 == r2
  | otherwise          = c1 == c2

walk :: UArray (Row, Col) Word16 -> (Row, Col) -> (Row, Col) -> Dir4 -> (Row, Col)
walk solid dest p d
  | d `into` (solid ! move4 d p) > 0 || intersect d dest (move4 d p) = move4 d p
  | otherwise = walk solid dest (move4 d p) d

startHeap :: UArray (Row, Col) Word16 -> (Row, Col) -> (Row, Col) -> Heap (Entry Int Path)
startHeap solid start dest = heap
  where
    startEntry d p = Heap.Entry (manhattan dest p + manhattan start p) (Path (manhattan start p) d [p, start])

    heap = Heap.fromList (startEntry <*> walk solid dest start <$> open (solid ! start))

entry :: Set (Row, Col)
      -> Path
      -> (Row, Col)
      -> (Row, Col)
      -> Dir4
      -> (Row, Col)
      -> Maybe (Entry Int Path)
entry visited path dest from d to
  | Set.member to visited = Nothing
  | otherwise = Just $
    Heap.Entry (distance path + manhattan from to + manhattan to dest)
      (Path (distance path + manhattan from to) d (to : nodes path))

type Visited = Set (Row, Col)

pathfind' :: UArray (Row, Col) Word16
          -> (Row, Col)
          -> (Visited, Heap (Entry Int Path))
          -> Either (Maybe Path) (Visited, Heap (Entry Int Path))
pathfind' _     _    (_, Heap.viewMin -> Nothing) = Left Nothing
pathfind' solid dest (visited, Heap.viewMin -> Just (Heap.Entry priority path, heap))
  | priority == distance path = Left (Just path)
  | otherwise =
      let p = head (nodes path)
          w = solid ! p
          h = hook (dir path) w
          newHeap dirs = Heap.fromList ((entry visited path dest p <*> walk solid dest p) `mapMaybe` dirs)
      in if | Set.member p visited ->
                pathfind' solid dest (visited, heap)
            | w .&. dirBit (dir path) > 0 ->
                Right $ (Set.insert p visited, heap `Heap.union` newHeap (openFrom (dir path) w))
            | h == 1 ->
                Right $ (Set.insert p visited, heap `Heap.union` newHeap [dir path, clock (dir path)])
            | h == 2 ->
                Right $ (Set.insert p visited, heap `Heap.union` newHeap [dir path, anticlock (dir path)])
            | h == 3 ->
                Right $ (Set.insert p visited, heap `Heap.union` newHeap [dir path, clock (dir path), anticlock (dir path)])
            | otherwise ->
                Right $ (Set.insert p visited, heap `Heap.union` newHeap (openFrom (dir path) w))

run :: (a -> Either b a) -> a -> b
run f x = case f x of
    (Left result) -> result
    (Right x')    -> run f x'

pathfind :: UArray (Row, Col) Word16 -> (Row, Col) -> (Row, Col) -> Maybe Path
pathfind solid start dest = run (pathfind' solid dest) (Set.insert start Set.empty, startHeap solid start dest)
