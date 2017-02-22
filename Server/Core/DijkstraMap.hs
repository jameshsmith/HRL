{-# LANGUAGE LambdaCase #-}
module Core.DijkstraMap
    ( DijkstraMap
    , mkDijkstraMap
    , downhill
    , unDMap
    ) where

import Prelude hiding ((.), id)

import Core.Types

import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.List (sortOn, nub)
import Data.Maybe

newtype DijkstraMap =
    DMap { unDMap :: (UArray (Row, Col) Word) } deriving (Eq, Show)

-- | Make a Dijkstra map from some list of goal tiles and an array of
-- solid/non-navigable tiles.
mkDijkstraMap :: [(Row, Col)] -> UArray (Row, Col) Bool -> DijkstraMap
mkDijkstraMap goals solid = DMap $ runSTUArray $ do
    dmap <- newArray (bounds solid) maxBound
    mkDijkstraMap' goals 0 solid dmap
    return dmap

mkDijkstraMap' :: [(Row, Col)] -> Word -> UArray (Row, Col) Bool -> STUArray s (Row, Col) Word -> ST s ()
mkDijkstraMap' [] _ _ _ = return ()
mkDijkstraMap' neighbors n solid dmap = do
    mapM_ (\i -> writeArray dmap i n) neighbors
    neighbors' <- concat <$> mapM (newNeighbors solid dmap) neighbors
    mkDijkstraMap' (nub neighbors') (n + 1) solid dmap

newNeighbors :: UArray (Row, Col) Bool -> STUArray s (Row, Col) Word -> (Row, Col) -> ST s [(Row, Col)]
newNeighbors solid dmap loc = do
    n <- readArray dmap north
    e <- readArray dmap east
    s <- readArray dmap south
    w <- readArray dmap west
    return $ mapMaybe checkNeighbor [(north, n), (east, e), (south, s), (west, w)]
  where
    north = move4 N4 loc
    east = move4 E4 loc
    south = move4 S4 loc
    west = move4 W4 loc
    
    checkNeighbor (loc', v) = if (solid ! loc') || (v /= maxBound) then Nothing else Just loc' 

-- | Roll downhill from a location on a Dijkstra map.
downhill :: (Row, Col) -> DijkstraMap -> [Dir4]
downhill i (DMap arr) =
    let dirs = sortOn snd . zip [N4 ..] $ map (ixD maxBound arr . flip move4 i) [N4 ..]
    in map fst $ takeWhile (\(_, h) -> h /= maxBound && pos >= h) dirs
  where
    pos = ixD maxBound arr i

{-
-- | Roll downhill on a list of weighted Dijkstra maps.
downhillN :: (Row, Col) -> [(Double, DijkstraMap)] -> [Dir4]
downhillN i wdmaps =
    let dirs = sortOn snd . zip [N4 ..] $ map (weight . flip move4 i) [N4 ..]
    in map fst $ takeWhile ((weight i >) . snd) dirs
  where
    weight j = sum $ map (\(w, (DMap arr)) -> fromIntegral (ixD maxBound arr j) * w) wdmaps
-}
