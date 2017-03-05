{-# LANGUAGE FlexibleContexts, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Core.FOV
    ( shadowCast
    , updateSeen
    , bresenham
    ) where

import Prelude hiding (lines)

import Core.Types (Row, Col, Dir8 (..))

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed

-- import Test.QuickCheck

slopeStart :: (Row, Col) -> Double
slopeStart (r, c) = (fromIntegral c - 0.5) / (fromIntegral r + 0.5)

slopeEnd :: (Row, Col) -> Double
slopeEnd (r, c) = (fromIntegral c + 0.5) / (fromIntegral r - 0.5)

slope :: (Row, Col) -> Double
slope (r, c) = fromIntegral c / fromIntegral r

type ScanLines = [[(Row, Col)]]

scanLines :: ScanLines
scanLines = map (\x -> map (x,) [x, (x-1) .. 0]) [1..]

scanEnd :: (Row, Col) -> ScanLines -> ScanLines
scanEnd n = map (filter ((> slopeEnd n) . slopeEnd))

scanStart :: (Row, Col) -> ScanLines -> ScanLines
scanStart n = map (filter ((< slopeStart n) . slopeStart))

rotate :: Dir8 -> (Row, Col) -> (Row, Col)
rotate N8  (x, y) = (-x, y)
rotate NE8 (x, y) = (-y, x)
rotate E8  (x, y) = (y, x)
rotate SE8 pos    = pos
rotate S8  (x, y) = (x, -y)
rotate SW8 (x, y) = (y, -x)
rotate W8  (x, y) = (-y, -x)
rotate NW8 (x, y) = (-x, -y)

rotateOp :: Dir8 -> Dir8
rotateOp NE8 = SW8
rotateOp SW8 = NE8
rotateOp d = d

unrotate :: Dir8 -> (Row, Col) -> (Row, Col)
unrotate d = rotate (rotateOp d) 

translate :: (Row, Col) -> (Row, Col) -> (Row, Col)
translate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

transform :: Dir8 -> (Row, Col) -> (Row, Col) -> (Row, Col)
transform dir off pos = translate (rotate dir pos) off

data Octant = Oct Int Dir8 (Row, Col)

scan :: Octant -> UArray (Row, Col) Bool -> STUArray s (Row, Col) Bool -> ScanLines -> ST s ()
scan _ _ _ ([] : _) = return ()
scan oct@(Oct dist dir off) opacity visible scans@((top:_) : _) = go scans
  where
    go ([] : lines) = scan oct opacity visible lines
    go ((pos@(r, _) : line) : lines)
      | r > dist = return ()

      | not (bounds opacity `inRange` toArray (r, 0)) = return ()

      | not (bounds opacity `inRange` toArray pos) = go (line : lines)

      | not (isOpaque pos) = makeVisible pos >> go (line : lines)

      | otherwise = do
          makeVisible pos
          when (pos /= top) $ scan oct opacity visible (scanEnd pos lines)
          let (wall, line') = span isOpaque line
          mapM_ makeVisible wall
          case line' of
            [] -> return ()
            ((r', c'):_) -> go $ scanStart (r', c'+1) (line' : lines)

    toArray = transform dir off

    makeVisible p = writeArray visible (toArray p) True

    isOpaque p = opacity ! toArray p

-- | Takes the position of the player and an array representing the
-- opacity of each tile in the map. Outputs an array such that visible
-- tiles are true, and obscured tiles are false.
shadowCast :: (Row, Col) -> UArray (Row, Col) Bool -> UArray (Row, Col) Bool
shadowCast start opacity = runSTUArray $ do
    visible <- newArray (bounds opacity) False
    forM_ [N8 .. NW8] $ \dir -> scan (Oct 100 dir start) opacity visible scanLines
    writeArray visible start True
    return visible

-- | For the first argument we take the array representing currently
-- visible tiles. The second argument is an array representing tiles
-- we have already seen. We return an updated version of the second
-- argument with the previously seen tiles and those now visible.
updateSeen :: UArray (Row, Col) Bool -> UArray (Row, Col) Bool -> UArray (Row, Col) Bool
updateSeen visible seen = runSTUArray $ do
    seen' <- thaw seen
    forM_ (indices seen) $ \i -> when (visible ! i) (writeArray seen' i True)
    return seen'

-- | Draw a bresenham line from a point to another point.  Returns a
-- list such that the first element is the first point, and the last
-- is the last element. The line is only guaranteed to be symmetric
-- when it contains an even number of points.
bresenham :: (Row, Col) -> (Row, Col) -> [(Row, Col)]
bresenham (r1, c1) start@(r0, c0) =
    translate start . unrotate oct <$> bresenham' (rotate oct (r1 - r0, c1 - c0))
  where
    oct = octant (r1 - r0, c1 - c0)

bresenham' :: (Row, Col) -> [(Row, Col)]
bresenham' end = go [] 0 (0, 0)
  where
    go acc err (r, c)
      | r == fst end           = (r, c) : acc
      | err + slope end >= 0.5 = go ((r, c) : acc) (err + slope end - 1) (r + 1, c + 1)
      | otherwise              = go ((r, c) : acc) (err + slope end) (r + 1, c)

octant :: (Row, Col) -> Dir8
octant (r, c)
  | r < 0  && c >= 0 && c <= (-r) = N8
  | r >= 0 && c < 0  && (-c) > r  = NE8
  | r >= 0 && c >= 0 && c > r     = E8
  | r >= 0 && c >= 0 && c <= r    = SE8
  | r >= 0 && c < 0  && (-c) <= r = S8
  | r < 0  && c >= 0 && c > (-r)  = SW8
  | r < 0  && c < 0  && c <= r    = W8
  | r < 0  && c < 0  && c > r     = NW8

-- ===== Quickcheck properties =====

{-
prop_octant pos =
  let (r, c) = rotate (octant pos) pos
  in r >= c && r >= 0 && c >= 0

prop_rotate dir pos = (unrotate dir . rotate dir) pos == pos

prop_bresEvenSym start end =
  let line = bresenham start end
  in if even (length line) then line == reverse (bresenham end start) else True

prop_bresNonEmpty start end = bresenham start end /= []

prop_bresHead start end = head (bresenham start end) == start

prop_bresLast start end = last (bresenham start end) == end

distFromLine :: (Row, Col) -> (Row, Col) -> (Row, Col) -> Double
distFromLine (r1, c1) (r2, c2) (rp, cp) =
  abs (fromIntegral ((r2 - r1)*cp - (c2 - c1)*rp + c2*r1 - r2*c1)) /
  sqrt (fromIntegral ((c2 - c1)^2 + (r2 - r1)^2))

prop_bresIsLine start end
  | start == end = True
  | otherwise = and ((< 0.5) . distFromLine start end <$> bresenham start end)
-}
