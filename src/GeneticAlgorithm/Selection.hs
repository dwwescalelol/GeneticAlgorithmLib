-- | Selection strategies for genetic algorithms.
module GeneticAlgorithm.Selection (
    Selection,
    tournementSelection,
    rselection,
    eliteSelection,
) where

import GeneticAlgorithm.Shared.Types (Selection)
import GeneticAlgorithm.Utility (segment)
import System.Random (Random (randomRs), mkStdGen)

-- | Roulette selection (infinite list, use 'take').
rselection :: Selection c
rselection seed evalPop = map (evalPop !!) indices
  where
    highestFitness = 1 + fst (last evalPop)
    accFitness = scanl1 (+) $ map ((highestFitness -) . fst) evalPop
    highestAccFitness = last accFitness
    randomNumbers = randomRs (0, highestAccFitness - 1) (mkStdGen seed)
    indices = map (indexOf accFitness) randomNumbers

-- | Find the index of the first accumulator value >= v.
indexOf :: (Ord a) => [a] -> a -> Int
indexOf xs v = length $ takeWhile (< v) xs

-- | Tournament selection (infinite list, use 'take').
tournementSelection :: (Ord c) => Selection c
tournementSelection seed evalPop = map (fitterChrom evalPop) (segment 2 indicies)
  where
    indicies = take (length evalPop * 2) $ randomRs (0, length evalPop - 1) (mkStdGen seed)
    fitterChrom evaledPop [i, j] = min (evaledPop !! i) (evaledPop !! j)
    fitterChrom _ _ = error "fitterChrom: expected exactly two indices"

-- | Elitist selection (returns population as-is, not infinite).
eliteSelection :: Selection c
eliteSelection _ evalPop = evalPop
