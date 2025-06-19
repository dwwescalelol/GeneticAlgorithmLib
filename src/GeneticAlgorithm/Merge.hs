-- | Merge strategies for combining populations in a genetic algorithm.
module GeneticAlgorithm.Merge (
    concatMerge,
    distinctOrderedMerge,
    orderedMerge,
) where

-- \^ Merge two ordered lists

import GeneticAlgorithm.Shared.Types (Merge)
import GeneticAlgorithm.Utility (distinct)

-- | Concatenate two populations.
concatMerge :: Merge c
concatMerge = (++)

-- | Merge two populations, remove duplicates, and sort.
distinctOrderedMerge :: (Ord c) => Merge c
distinctOrderedMerge xs ys = orderedMerge (distinct xs) (distinct ys)

-- | Merge two ordered populations into a single ordered population.
orderedMerge :: (Ord c) => Merge c
orderedMerge xs [] = xs
orderedMerge [] ys = ys
orderedMerge (x : xs) (y : ys)
    | x <= y = x : orderedMerge xs (y : ys)
    | otherwise = y : orderedMerge (x : xs) ys
