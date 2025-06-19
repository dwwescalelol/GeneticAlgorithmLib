{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Core types and functions for the genetic algorithm framework.
module GeneticAlgorithm (
    -- Types
    Fitness,
    MkRand,
    InitialPopulation (..),
    GAConfig (..),
    EvolveMeta (..),
    -- Functions
    assignOperatorChunks,
    evalPop,
    evolve,
    initPop,
    makeEvolveMeta,
    loop,
    geneticAlgorithm,
) where

import Data.List (sortBy)
import GeneticAlgorithm.GeneticOpperators (OpperatorConfig (..))
import GeneticAlgorithm.Shared.Types (
    Eval,
    FitnessValue,
    Merge,
    Pop,
    Seed,
    Selection,
    Size,
    Stop,
 )
import GeneticAlgorithm.Utility (compareEval, segment)
import System.Random (Random (randomRs), mkStdGen)

-- | Fitness function type alias.
type Fitness c = c -> FitnessValue

-- | Random chromosome generator type alias.
type MkRand c = Seed -> c

-- | Initial population specification.
data InitialPopulation c = RandChrom (MkRand c) | InitialisedPop (Pop c)

-- | Main configuration for a GA run.
data GAConfig c = GAConfig
    { maxGenerations :: Int
    , popSize :: Int
    , chromSize :: Int
    , initialPopulation :: InitialPopulation c
    , fitness :: Fitness c
    , selection :: Selection c
    , operators :: [OpperatorConfig c]
    , merge :: Merge c
    , stop :: Stop c
    }

-- | Metadata for a single evolution step.
type TotalNumParents = Int

-- | Bundles operator and GA context for one generation.
data EvolveMeta c = EvolveMeta
    { operatorMeta :: [(TotalNumParents, OpperatorConfig c)]
    , selection :: Selection c
    , merge :: Merge c
    , stop :: Stop c
    , chromSize :: Int
    , fitness :: Fitness c
    , popSize :: Int
    }

-- | Assigns parent groups to each operator for a generation.
assignOperatorChunks :: EvolveMeta c -> [a] -> Seed -> [(OpperatorConfig c, [[a]], Seed)]
assignOperatorChunks EvolveMeta{..} selectedPop seed =
    zip3 (map snd operatorMeta) chunkGroups seeds
  where
    totalNumParents = map fst operatorMeta
    seeds = randomRs (0, maxBound) (mkStdGen seed) :: [Int]
    chunks = [take n $ drop offset selectedPop | (n, offset) <- zip totalNumParents (scanl (+) 0 totalNumParents)]
    chunkGroups = zipWith segment (map (numParents . snd) operatorMeta) chunks

-- | Evaluate and sort a population by fitness.
evalPop :: (Ord c) => (c -> FitnessValue) -> [c] -> [Eval c]
evalPop fitness pop = sortBy compareEval $ zip (map fitness pop) pop

-- | Run one generation: selection, variation, merge, and survivor selection.
evolve :: (Ord c) => EvolveMeta c -> Pop (Eval c) -> Seed -> Pop (Eval c)
evolve EvolveMeta{..} evaledPop seed =
    take popSize $ foldl merge [] (offspringEvalPops ++ [persistingPop])
  where
    selectedPop = map snd $ selection seed evaledPop
    zippedChunks = assignOperatorChunks EvolveMeta{..} selectedPop seed
    offspringPops = [concatMap (opperator op chromSize opSeed) groups | (op, groups, opSeed) <- zippedChunks]
    offspringEvalPops = [evalPop fitness pop | pop <- offspringPops]
    persistingPop = evalPop fitness $ take popSize $ drop (popSize - sum (map length offspringEvalPops)) selectedPop

-- | Generate a random population.
initPop :: Size -> MkRand c -> Seed -> Pop c
initPop popSize mkRandChrom seed = map mkRandChrom seeds
  where
    rnds = randomRs (0, maxBound :: Seed) (mkStdGen seed)
    seeds = take popSize rnds

-- | Build EvolveMeta from GAConfig for a generation.
makeEvolveMeta :: GAConfig c -> EvolveMeta c
makeEvolveMeta GAConfig{..} =
    EvolveMeta
        { operatorMeta = zip opParentCounts operators
        , selection = selection
        , merge = merge
        , stop = stop
        , chromSize = chromSize
        , fitness = fitness
        , popSize = popSize
        }
  where
    opParentCounts = map (\op -> calcParents (proportion op) (numParents op) (numChildren op)) operators
    calcParents prop nParents nChildren =
        nParents * floor ((fromIntegral popSize * prop) / fromIntegral nChildren)

-- | Evolve a population for up to maxGenerations or until stop.
loop :: [Pop (Eval c) -> Pop (Eval c)] -> Stop c -> Pop (Eval c) -> [Pop (Eval c)]
loop [] _ x = [x]
loop (f : fs) stop x
    | stop x = [x]
    | otherwise = x : loop fs stop (f x)

-- | Run a full genetic algorithm, returning all populations.
geneticAlgorithm :: (Ord c) => GAConfig c -> Seed -> [Pop (Eval c)]
geneticAlgorithm
    GAConfig{..}
    seed =
        map (take popSize) evolvedPop
      where
        seeds = randomRs (0, maxBound) (mkStdGen seed)
        initialPop
            | RandChrom mkRandChrom <- initialPopulation = evalPop fitness (initPop popSize mkRandChrom (seeds !! (maxGenerations + 1)))
            | InitialisedPop pop <- initialPopulation = evalPop fitness pop
        evolvedPop =
            loop
                (map ((flip . evolve . makeEvolveMeta) GAConfig{..}) (take maxGenerations seeds))
                stop
                initialPop
