module Server.Input where

import Core.Types
import Abyss.Game

import Data.Maybe (fromJust)

data Trie a = Leaf a
            | Nodes [(Char, Trie a)]
            deriving Show

instance Functor Trie where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Nodes xs) = Nodes $ map (\(c, t) -> (c, fmap f t)) xs

trie :: [Char] -> a -> Trie a
trie [] x = Leaf x
trie (c : cs) x = Nodes [(c, trie cs x)]

merge :: Trie a -> Trie a -> Maybe (Trie a)
merge (Nodes xs) (Nodes []) = Just (Nodes xs)
merge (Nodes []) (Nodes ys) = Just (Nodes ys)
merge (Nodes (x : xs)) (Nodes (y : ys))
  | fst x < fst y  = do { Nodes ns <- merge (Nodes xs) (Nodes (y : ys)); return (Nodes (x : ns)) } 
  | fst x > fst y  = do { Nodes ns <- merge (Nodes (x : xs)) (Nodes ys); return (Nodes (y : ns)) }
  | fst x == fst y = do { t <- merge (snd x) (snd y); Nodes ns <- merge (Nodes xs) (Nodes ys); return (Nodes ((fst x, t) : ns)) }
merge _ _ = Nothing

mergeList :: [Trie a] -> Maybe (Trie a)
mergeList (x : []) = Just x
mergeList (x : x' : xs) = do { x'' <- merge x x'; mergeList (x'' : xs) }
mergeList _ = Nothing

commands' :: Trie Action
commands' = fromJust $ mergeList
    [ trie "w" (Move N4)
    , trie "d" (Move E4)
    , trie "s" (Move S4)
    , trie "a" (Move W4)
    , trie " " Skip
    , trie "tw" (Activate N4)
    , trie "td" (Activate E4)
    , trie "ts" (Activate S4)
    , trie "ta" (Activate W4)
    ]

commands = fromJust $ commands' `merge` (Nodes [('i', fmap Confirm commands')])

complete :: Trie Action -> [Char] -> Maybe (Either [Char] Action)
complete (Leaf x) [] = Just (Right x)
complete (Leaf _) _ = Nothing
complete (Nodes xs) [] = Just (Left (map fst xs))
complete (Nodes xs) (c : cs) = lookup c xs >>= flip complete cs
