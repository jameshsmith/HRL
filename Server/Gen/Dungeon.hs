{-# LANGUAGE ViewPatterns #-}
module Gen.Dungeon (plainRoom, printDungeon, dungeonGen, dungeonGen') where

import Core.Types
import Core.ZArray

import Prelude hiding ((.), id)

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Char
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.Random

type Walls = [Dir4]

data Cell = Room Int | Corridor Walls

upTo :: Monad m => Int -> StateT StdGen m Int
upTo n = StateT (return . randomR (0, n))

pick :: (Functor m, Monad m) => [a] -> StateT StdGen m (Maybe a)
pick [] = return Nothing
pick xs@(length -> l) = (Just . (xs !!)) <$> upTo (l - 1)

maze :: Array (Row, Col) Int -> State StdGen (Array (Row, Col) Cell)
maze rooms = state $ \gen -> runST $ runStateT (mkMaze rooms) gen

toCells :: Array (Row, Col) Int -> Array (Row, Col) Cell
toCells = amap (\r -> if r == 0 then Corridor [] else Room r)

mkMaze ::  Array (Row, Col) Int -> StateT StdGen (ST s) (Array (Row, Col) Cell)
mkMaze rooms = do
    Just (startR, startC) <- pick . map fst . filter ((== 0) . snd) $ assocs rooms
    mz <- lift . thaw $ toCells rooms
    genMaze [(startR, startC)] mz
    lift $ freeze mz

genMaze :: [(Row, Col)] -> STArray s (Row, Col) Cell -> StateT StdGen (ST s) ()
genMaze [] _ = return ()
genMaze stack@(cur:prev) mz = do
    sel <- pick . filter unvisited =<< neighbors cur mz
    case sel of
      Nothing -> genMaze prev mz
      Just (dir, _) -> do
        breakWall cur dir mz
        genMaze ((move4 dir cur):stack) mz

breakWall :: (Row, Col) -> Dir4 -> STArray s (Row, Col) Cell -> StateT StdGen (ST s) ()
breakWall pos dir mz = lift $ do
  Corridor walls1 <- readArray mz pos
  writeArray mz pos (Corridor (dir : walls1))
  Corridor walls2 <- readArray mz (move4 dir pos)
  writeArray mz (move4 dir pos) (Corridor (opposite4 dir : walls2))

neighbors :: (Row, Col) -> STArray s (Row, Col) Cell -> StateT StdGen (ST s) [(Dir4, Walls)]
neighbors pos mz = lift $ catMaybes <$> do
    bnds <- getBounds mz
    let coords = filter (inRange bnds . snd) $ map (id &&& flip move4 pos) [N4 ..]
    forM coords $ \(d, i) -> do
      val <- readArray mz i
      case val of
        (Corridor walls) -> return $ Just (d, walls)
        (Room _) -> return $ Nothing

unvisited :: (Dir4, Walls) -> Bool
unvisited (_, []) = True
unvisited _ = False

data Tile = Wall | CFloor | RFloor Int

tileToChar :: Tile -> Char
tileToChar Wall = '#'
tileToChar CFloor = ' '
tileToChar (RFloor n) = chr (n + ord 'a' - 1)

cells :: Cell -> Array (Row, Col) Tile
cells (Room n) = listArray ((0,0), (1,1)) . repeat $ RFloor n
cells (Corridor walls) =
    amap toChar . listArray ((0,0), (1,1)) $ foldr (zipWith (||)) (repeat False) (map corridor walls)
  where
    corridor N4 = [False,True,False,True]
    corridor E4 = [False,False,False,True]
    corridor S4 = [False,False,False,True]
    corridor W4 = [False,False,True,True]

    toChar True = CFloor
    toChar False = Wall

cellularize :: Array (Row, Col) Cell -> Array (Row, Col) Tile
cellularize (amap cells -> mz) = fixRooms $ runSTArray $ do
    let (_, (r, c)) = bounds mz
        (nr, nc) = ((r + 1) * 2, (c + 1) * 2)
    cmaze <- newArray ((0,0), (nr,nc)) Wall
    forM_ (range ((0,0), (nr - 1, nc - 1))) $ \(pos@(ind (2,2) -> (o, i))) -> do
        writeArray cmaze pos (mz ! o ! i)
    return cmaze

fixRooms' :: ZArray (Row, Col) Tile -> Tile
fixRooms' zarr
  | not (isRoomFloor (zgo N4 zarr)), (RFloor _) <- extract zarr = Wall
  | not (isRoomFloor (zgo W4 zarr)), (RFloor _) <- extract zarr = Wall
  | otherwise = extract zarr
  where
    isRoomFloor (Just (RFloor _)) = True
    isRoomFloor _ = False

fixRooms :: Array (Row, Col) Tile -> Array (Row, Col) Tile
fixRooms = unZArray . extend fixRooms' . ZArray (0,0)

ind :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
ind (ir, ic) (r, c) = reArrange (r `divMod` ir, c `divMod` ic)
  where
    reArrange ((x,y),(a,b)) = ((x,a), (y,b))

zgo :: Dir4 -> ZArray (Row, Col) e -> Maybe e
zgo d (ZArray i arr) | bounds arr `inRange` move4 d i = Just (arr ! move4 d i)
zgo _ _ = Nothing

printDungeon :: UArray (Row, Col) Char -> IO ()
printDungeon arr = do
    let (_, (maxR, maxC)) = bounds arr
    forM_ [0..maxR] $ \r -> do
        forM_ [0..maxC] $ \c -> do
            putChar $ (arr ! (r, c))
        putChar '\n'

checkLoc :: (Row, Col) -> ZArray (Row, Col) Int -> Bool
checkLoc (_, _) (ZArray (0, _) _) = False
checkLoc (_, _) (ZArray (_, 0) _) = False
checkLoc (h, w) (ZArray (r, c) arr) =
    and . map ((==) (Just 0) . (?) arr) $ range ((r - 1, c - 1), (r + h, c + w))

mkZArray :: Array (Row, Col) e -> ZArray (Row, Col) e
mkZArray = ZArray (0,0)

asZArray :: (ZArray (Row, Col) a -> ZArray (Row, Col) b) -> Array (Row, Col) a -> Array (Row, Col) b
asZArray f = unZArray . f . mkZArray

placeRooms :: Int
           -> [(Row, Col)]
           -> Array (Row, Col) Int
           -> State StdGen (Array (Row, Col) Int)
placeRooms _ [] arr = return arr
placeRooms n ((h, w):rooms) arr = do
    let candidates = asZArray (extend (checkLoc (h, w))) arr 
    mloc <- pick . map fst . filter ((==) True . snd) $ assocs candidates
    case mloc of
      Nothing -> return arr
      Just loc -> do
        placeRooms (n + 1) rooms $ runSTArray $ do
          marr <- thaw arr
          forM_ (range (loc, (fst loc + h - 1, snd loc + w - 1))) $ \i -> writeArray marr i n
          return marr

plainRoom :: (Int, Int) -> [[Char]]
plainRoom (r, c)
  | even r = concat [[top], replicate (r - 2) walls, [middle], replicate r walls, [top]]
  | otherwise = concat [[top], replicate (r - 1) walls, [middle], replicate (r - 1) walls, [top]]
  where
    top
      | even c = replicate (c - 1) '#' ++ "+" ++ replicate (c + 1) '#'
      | otherwise = replicate c '#' ++ "+" ++ replicate c '#'
    walls = "#" ++ replicate (c * 2 - 1) ' ' ++ "#"
    middle = "+" ++ replicate (c * 2 - 1) ' ' ++ "+"

roomSize :: [[Char]] -> (Int, Int)
roomSize [] = error "Not a valid room!"
roomSize room@(top:_) = (length room `div` 2, length top `div` 2) 

corners' :: Array (Row, Col) Tile -> (Row, Col) -> Map Int (Row, Col) -> Map Int (Row, Col)
corners' dungeon pos found
  | (RFloor n) <- (dungeon ! pos), Map.notMember n found = Map.insert n pos found
  | otherwise = found

corners :: Array (Row, Col) Tile -> [(Row, Col)]
corners dungeon = Map.elems $ foldl (flip ($)) Map.empty (map (corners' dungeon) (indices dungeon))

roomUpdates' :: (Row, Col) -> Row -> [[Char]] -> [((Row, Col), Char)]
roomUpdates' (_, _) _ [] = []
roomUpdates' (r, c) _ ([]:ys) = roomUpdates' (r + 1, c) c ys
roomUpdates' (r, c) cAcc ((x:xs):ys) = ((r - 1, cAcc - 1), x) : roomUpdates' (r, c) (cAcc + 1) (xs:ys)

roomUpdates :: (Row, Col) -> [[Char]] -> [((Row, Col), Char)]
roomUpdates (r, c) room = roomUpdates' (r, c) c room 

dungeonGen :: [[[Char]]] -> State StdGen (UArray (Row, Col) Char)
dungeonGen rooms = do
    let zeros = listArray ((0, 0), (19, 19)) (repeat 0)
    tiles <- cellularize <$> (maze =<< placeRooms 1 (map roomSize rooms) zeros)
    let barr = asZArray removeDeadEnds $ (fmap tileToChar tiles) // concat (zipWith roomUpdates (corners tiles) rooms)
    return $ array (U.bounds barr) (U.assocs barr)

dungeonGen' :: [[[Char]]] -> (Row, Col) -> State StdGen (UArray (Row, Col) Char)
dungeonGen' rooms (mR, mC) = do
    let zeros = listArray ((0, 0), (mR, mC)) (repeat 0)
    tiles <- cellularize <$> (maze =<< placeRooms 1 (map roomSize rooms) zeros)
    let barr = (fmap tileToChar tiles) // concat (zipWith roomUpdates (corners tiles) rooms)
    return $ array (U.bounds barr) (U.assocs barr)
    
z4 :: ZArray (Row, Col) e -> [e]
z4 zarr = catMaybes $ map (flip zgo zarr) [N4, E4, S4, W4]

deadEnd :: ZArray (Row, Col) Char -> Char
deadEnd zarr
  | sort (z4 zarr) == " ###" && extract zarr == ' ' = '#'
  | otherwise = extract zarr

removeDeadEnds :: ZArray (Row, Col) Char -> ZArray (Row, Col) Char
removeDeadEnds zarr
  | unZArray zarr' == unZArray zarr = zarr
  | otherwise = removeDeadEnds zarr'
  where
    zarr' = extend deadEnd zarr
                                                                                         
