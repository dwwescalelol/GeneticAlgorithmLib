import Data.List (tails)
import GeneticAlgorithm (
    Fitness,
    GAConfig (..),
    InitialPopulation (RandChrom),
    MkRand,
    geneticAlgorithm,
 )
import GeneticAlgorithm.GeneticOpperators
import GeneticAlgorithm.Merge
import GeneticAlgorithm.Selection
import GeneticAlgorithm.Shared.Types
import GeneticAlgorithm.Stop (stopExact)
import GeneticAlgorithm.Utility

type NQueen = Int
type Row = Int
type Column = Int
type Board = [Column]
type Board2D = [(Row, Column)]

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1 .. size]

qfitness :: Fitness Board
qfitness b = length $ concatMap f ((init . tails) (to2DBoard b))
  where
    f (q : qs) = filter (takes q) qs
    f [] = []

to2DBoard :: Board -> Board2D
to2DBoard = zip [1 ..]

takes :: (Row, Column) -> (Row, Column) -> Bool
takes (r1, c1) (r2, c2) = abs (r1 - r2) == abs (c1 - c2)

gaForQueens :: Int -> Int -> Int -> (Float, Float) -> Seed -> [Pop (Eval Board)]
gaForQueens n mg ps (xPro, mPro) =
    geneticAlgorithm
        GAConfig
            { maxGenerations = mg
            , popSize = ps
            , chromSize = n
            , initialPopulation = RandChrom (randQueen n)
            , fitness = qfitness
            , selection = rselection
            , operators = [permCrossoverConfig xPro, mutationBySwapConfig mPro]
            , merge = distinctOrderedMerge
            , stop = stopExact 0
            }

mainn :: Int -> IO ()
mainn n = do
    let seed = 123456
        mg = 50
        ps = 500
        xProb = 0.4
        mProb = 0.4
        solutions = gaForQueens n mg ps (xProb, mProb) seed
    print solutions
