{-# LANGUAGE DisambiguateRecordFields #-}

import GeneticAlgorithm
import GeneticAlgorithm.GeneticOpperators
import GeneticAlgorithm.Merge
import GeneticAlgorithm.Selection (rselection)
import GeneticAlgorithm.Shared.Types
import GeneticAlgorithm.Stop (fitnessStop)

type TestType = [Int]

fitnessFn :: TestType -> Int
fitnessFn = maximum

addOne :: ChromSize -> Seed -> [TestType] -> [TestType]
addOne _ _ xs =
    case xs of
        ((y : ys) : rest) -> ((y + 1) : ys) : rest
        [] -> []
        ([] : rest) -> [] : rest

-- Dummy operator: identity (does nothing)
dummyOp :: Float -> OpperatorConfig TestType
dummyOp pro =
    OpperatorConfig
        { opperator = addOne
        , proportion = pro
        , numParents = 1
        , numChildren = 1
        }

dummyOp2 :: Float -> OpperatorConfig TestType
dummyOp2 pro =
    OpperatorConfig
        { opperator = addOne
        , proportion = pro
        , numParents = 1
        , numChildren = 1
        }

-- Initial population: 5 chromosomes of length 3
initialChroms :: Pop TestType
initialChroms = replicate 10 [1]

-- InitialPopulation wrapper
initialPop :: InitialPopulation TestType
initialPop = InitialisedPop initialChroms

-- Build GAConfig
testGAConfig :: GAConfig TestType
testGAConfig =
    GAConfig
        { maxGenerations = 10
        , popSize = 10
        , chromSize = 3
        , initialPopulation = initialPop
        , fitness = fitnessFn
        , selection = rselection
        , operators = [dummyOp 0.5, dummyOp2 0.5]
        , merge = concatMerge
        , stop = fitnessStop 20
        }

testEvolveMeta :: EvolveMeta TestType
testEvolveMeta = makeEvolveMeta testGAConfig

-- Prepare initial evaluated population
testEvalPop :: Pop (Eval TestType)
testEvalPop = evalPop fitnessFn initialChroms

-- Run evolve with a fixed seed
testEvolve :: IO ()
testEvolve = do
    let result = evolve testEvolveMeta testEvalPop 1
    putStrLn "[Test] Resulting population:"
    print result

main :: IO ()
main = testEvolve
