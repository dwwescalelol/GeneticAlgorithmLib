-- | Core type aliases for genetic algorithm components.
module GeneticAlgorithm.Shared.Types (
    Size,
    GeneticOpperator,
    Merge,
    Stop,
    Selection,
    ChromSize,
    FitnessValue,
    Eval,
    Seed,
    Pop,
) where

-- | Number of genes in a chromosome.
type ChromSize = Int

-- | Population size.
type Size = Int

-- | Fitness value (higher is better by convention).
type FitnessValue = Int

-- | Chromosome with its fitness.
type Eval c = (FitnessValue, c)

-- | Population of chromosomes.
type Pop c = [c]

-- | Random seed.
type Seed = Int

-- | Selection function: returns (potentially infinite) list of selected individuals.
type Selection c = Seed -> [Eval c] -> [Eval c]

-- | Genetic operator: e.g., crossover or mutation.
type GeneticOpperator c = ChromSize -> Seed -> [c] -> [c]

-- | Merge two populations (e.g., for survivor selection).
type Merge c = Pop (Eval c) -> Pop (Eval c) -> Pop (Eval c)

-- | Stopping condition for evolution.
type Stop c = Pop (Eval c) -> Bool
