-- | Stopping conditions for genetic algorithms.
module GeneticAlgorithm.Stop (
    fitnessStop,
    dontStop,
) where

import GeneticAlgorithm.Shared.Types (Stop)

-- | Stop if the best fitness equals n.
fitnessStop :: Int -> Stop c
fitnessStop _ [] = True
fitnessStop n (p : _) = fst p <= n

dontStop :: Int -> Stop c
dontStop _ [] = True
dontStop _ (p : _) = False