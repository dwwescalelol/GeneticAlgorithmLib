-- | Utility functions for population handling and randomization in GA.
module GeneticAlgorithm.Utility (
    distinct,
    compareEval,
    adjacentPairs,
    shuffle,
    segment,
) where

import GeneticAlgorithm.Shared.Types (Eval, Seed)
import System.Random (Random (randomRs), mkStdGen)

-- | Remove duplicates from a sorted population (by fitness, then chromosome).
distinct :: (Ord c) => [Eval c] -> [Eval c]
distinct [] = []
distinct [x] = [x]
distinct (x : y : xs)
    | compareEval x y == EQ = distinct (x : xs)
    | otherwise = x : distinct (y : xs)

-- | Compare two evaluated chromosomes by fitness, then chromosome value.
compareEval :: (Ord c) => Eval c -> Eval c -> Ordering
compareEval (f1, c1) (f2, c2)
    | f1 < f2 = LT
    | f1 > f2 = GT
    | f1 == f2 && c1 < c2 = LT
    | f1 == f2 && c1 > c2 = GT
    | otherwise = EQ

-- | Return consecutive pairs from a list.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs [_] = []
adjacentPairs (x : y : xs) = (x, y) : adjacentPairs xs

-- | Split a list into chunks of size n.
segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment n xs = take n xs : segment n (drop n xs)

-- | Deterministically shuffle a list using a seed.
shuffle :: Seed -> [a] -> [a]
shuffle seed xs = shuffle' seeds xs
  where
    seeds = randomRs (0, length xs - 1) (mkStdGen seed)
    shuffle' _ [] = []
    shuffle' _ [x] = [x]
    shuffle' [] _ = []
    shuffle' (s : ss) qs = q : shuffle' ss remaining
      where
        index = s `mod` length qs
        q = qs !! index
        remaining = take index qs ++ drop (index + 1) qs
