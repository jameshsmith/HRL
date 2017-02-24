module Main where

import Prelude hiding ((.), id)

import Core.Types
import Core.BPS
import Core.DijkstraMap
import Gen.Dungeon

import Control.DeepSeq
import Control.Monad.Trans.State
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Word
import System.Random

import Criterion.Main

rooms :: [(Int, Int)]
rooms = [(6,7),(6,5),(4,4),(3,4),(4,2)] ++ concat (repeat [(3,3),(2,2),(2,2),(2,1)])

setupEnv :: IO (UArray (Row, Col) Bool, UArray (Row, Col) Word16, (Row, Col), (Row, Col))
setupEnv = do
    gen <- newStdGen
    let (charDungeon, gen') = runState (dungeonGen (map plainRoom rooms)) gen
        boolDungeon = amap (\c -> if c == '#' then True else False) charDungeon
        wordDungeon = preProcess boolDungeon
        freeSpots = map fst $ filter (not . snd) (assocs boolDungeon)
        (s1, gen'') = randomR (0, length freeSpots - 1) gen'
        (s2, _) = randomR (0, length freeSpots - 1) gen''
    return (boolDungeon, wordDungeon, freeSpots !! s1, freeSpots !! s2)
    
main :: IO ()
main = do
    (d1, d2, start, dest) <- setupEnv
    putStrLn ("Going from " ++ show start ++ " dest " ++ show dest) 
    putStrLn (deepseq (start, dest) "About to Bench...")
    putStrLn "=== AStar Path ==="
    putStrLn (show (astar d1 start dest))
    putStrLn "=== BPS Path ==="
    putStrLn (show (pathfind d2 start dest))
    putStrLn "=== BPS ST Path ==="
    putStrLn (show (pathfind2 d2 start dest))
    putStrLn "=== BPS2 Path ==="
    putStrLn (show (pathfind3 d2 start dest))
    putStrLn "=== Dijkstra Floodfill ==="
    putStrLn (show (unDMap (mkDijkstraMap [(dest)] d1) ! start))
    defaultMain
        [ bgroup "pathfinding"
           [ bench "A*" $ whnf (astar d1 start) dest
           , bench "BPS" $ whnf (pathfind d2 start) dest
           , bench "BPS in ST" $ whnf (pathfind2 d2 start) dest
           , bench "BPS2" $ whnf (pathfind3 d2 start) dest
           , bench "BPS precompute step" $ whnf preProcess d1
           , bench "DijkstraMap" $ whnf (mkDijkstraMap [(dest)]) d1
           ]
        ]
