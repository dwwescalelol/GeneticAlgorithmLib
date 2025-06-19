-- | Stopping conditions for genetic algorithms.
module GeneticAlgorithm.Stop (
    stopExact,
    stopMin,
    stopMax,
) where

import GeneticAlgorithm.Shared.Types (Stop)

-- | Stop if the best fitness equals n.
stopExact :: Int -> Stop c
stopExact _ [] = True
stopExact n (p : _) = fst p == n

-- | Stop if the best fitness is less than n.
stopMin :: Int -> Stop c
stopMin _ [] = True
stopMin n (p : _) = fst p < n

-- | Stop if the best fitness is greater than n.
stopMax :: Int -> Stop c
stopMax _ [] = True
stopMax n (p : _) = fst p > n