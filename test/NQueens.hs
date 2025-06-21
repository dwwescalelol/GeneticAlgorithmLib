module NQueens (main) where

import GeneticAlgorithm (
    Fitness,
    GAConfig (..),
    InitialPopulation (RandChrom),
    MkRand,
    geneticAlgorithm,
 )
import GeneticAlgorithm.GeneticOpperators (
    mutationBySwapConfig,
    permCrossoverConfig,
 )
import GeneticAlgorithm.Merge (distinctOrderedMerge)
import GeneticAlgorithm.Selection (rselection)
import GeneticAlgorithm.Shared.Types (Eval, Pop, Seed)
import GeneticAlgorithm.Stop (fitnessStop)
import GeneticAlgorithm.Utility (shuffle)

type Board = [Int]

mkRandQueen :: Int -> MkRand Board
mkRandQueen n seed = shuffle seed [1 .. n]

qFitness :: Fitness Board
qFitness board = length (filter diagonalConflict pairs)
  where
    queenPositions = zip [1 ..] board -- [(Row, Column)]
    pairs = allPairs queenPositions

-- Check if two queens are in conflict
diagonalConflict :: ((Int, Int), (Int, Int)) -> Bool
diagonalConflict ((row1, col1), (row2, col2)) =
    abs (row1 - row2) == abs (col1 - col2)

-- Generate all unique (q1, q2) pairs from the list
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, y) | y <- xs] ++ allPairs xs

gaForQueens :: Int -> Int -> Int -> (Float, Float) -> Seed -> [Pop (Eval Board)]
gaForQueens n mg ps (xPro, mPro) =
    geneticAlgorithm
        GAConfig
            { maxGenerations = mg
            , popSize = ps
            , chromSize = n
            , initialPopulation = RandChrom (mkRandQueen n)
            , fitness = qFitness
            , selection = rselection
            , operators = [permCrossoverConfig xPro, mutationBySwapConfig mPro]
            , merge = distinctOrderedMerge
            , stop = fitnessStop 0
            }

main :: Int -> IO ()
main n = do
    let seed = 123456
        mg = 50
        ps = 500
        xProb = 0.4
        mProb = 0.4
        solutions = gaForQueens n mg ps (xProb, mProb) seed
    print solutions
