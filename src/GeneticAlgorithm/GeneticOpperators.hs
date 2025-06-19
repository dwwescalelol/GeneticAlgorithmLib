-- | Genetic operators and operator configuration for GAs.
module GeneticAlgorithm.GeneticOpperators (
    OpperatorConfig (..),
    onePointCrossoverConfig,
    permCrossoverConfig,
    permCrossover2Config,
    mutationBySwapConfig,
) where

import Data.List ((\\))
import GeneticAlgorithm.Shared.Types (ChromSize, GeneticOpperator, Seed)
import System.Random (Random (randomRs), mkStdGen)

-- | Configuration for a genetic operator.
data OpperatorConfig c = OpperatorConfig
    { opperator :: ChromSize -> Seed -> [c] -> [c]
    -- ^ Operator function
    , proportion :: Float
    -- ^ Proportion of population
    , numParents :: Int
    -- ^ Parents per application
    , numChildren :: Int
    -- ^ Children per application
    }

--
-- Crossover operators
--

-- | Single-point crossover operator function for lists.
onePointCrossover :: GeneticOpperator [a]
onePointCrossover size seed [xs, ys] = [take i xs ++ drop i ys]
  where
    i = mod seed size
onePointCrossover _ _ _ = []

-- | Configuration for single-point crossover.
onePointCrossoverConfig :: Float -> OpperatorConfig [a]
onePointCrossoverConfig pro =
    OpperatorConfig
        { opperator = onePointCrossover
        , proportion = pro
        , numParents = 2
        , numChildren = 1
        }

-- | Permutation crossover: prefix from xs, rest from ys.
permCrossover :: (Eq a) => GeneticOpperator [a]
permCrossover size seed [xs, ys] = [cs ++ (ys \\ cs)]
  where
    cs = take i xs
    i = mod seed size
permCrossover _ _ _ = []

-- | Configuration for single-point crossover.
permCrossoverConfig :: (Eq a) => Float -> OpperatorConfig [a]
permCrossoverConfig pro =
    OpperatorConfig
        { opperator = permCrossover
        , proportion = pro
        , numParents = 2
        , numChildren = 1
        }

-- | Two-way permutation crossover: both directions.
permCrossover2 :: (Eq a) => GeneticOpperator [a]
permCrossover2 size seed [xs, ys] = [csA ++ (ys \\ csA), csB ++ (xs \\ csB)]
  where
    i = mod seed size
    csA = take i xs
    csB = take i ys
permCrossover2 _ _ _ = [] -- covers all other patterns

-- | Configuration for single-point crossover.
permCrossover2Config :: (Eq a) => Float -> OpperatorConfig [a]
permCrossover2Config pro =
    OpperatorConfig
        { opperator = permCrossover2
        , proportion = pro
        , numParents = 2
        , numChildren = 1
        }

--
-- Mutation operators
--

-- | Mutation by swapping two random elements.
mutationBySwap :: GeneticOpperator [a]
mutationBySwap size seed [xs] = case rs of
    (i : j : _) -> [swapItemAt (i, j) xs]
    _ -> [xs]
  where
    rs = randomRs (0, size - 1) (mkStdGen seed)
mutationBySwap _ _ _ = []

-- | Configuration for single-point crossover.
mutationBySwapConfig :: Float -> OpperatorConfig [a]
mutationBySwapConfig pro =
    OpperatorConfig
        { opperator = onePointCrossover
        , proportion = pro
        , numParents = 1
        , numChildren = 1
        }

-- | Swap two elements at given indices.
swapItemAt :: (Int, Int) -> [a] -> [a]
swapItemAt (i, j) xs
    | i == j = xs
    | otherwise = map pick [0 .. length xs - 1]
  where
    (a, b) = (min i j, max i j)
    pick k
        | k == a = xs !! b
        | k == b = xs !! a
        | otherwise = xs !! k