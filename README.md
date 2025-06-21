# GeneticAlgorithmLib

A flexible, functional genetic algorithm library written in Haskell.

## Philosophy

Genetic algorithms are typically structured around predefined generic patterns for crossover and mutation specific to a given encoding. These patterns work well in many cases, but they can limit the ability to shape the algorithm around the specific needs of a problem.

`GeneticAlgorithmLib` was built around the idea that the ingenuity of a great GA lies not in fitting the problem to the algorithm, but in shaping the algorithm around the problem. To support this, the library enables the creation of fully custom genetic operators, selection methods and the introduction of  a new merge operator letting developers design strategies that reflect the structure and demands of their specific domain.

Mutation and crossover serve distinct purposes: exploration and recombination, respectively, yet both can be expressed as functions from one or more parents to one or more offspring. This library embraces that formal symmetry while respecting their conceptual differences. Users can define multiple crossover and mutation strategies, both standard and custom, and specify their exact proportions in each generation. This design gives developers the freedom to shape operator behavior around the structure of their problem, rather than forcing the problem to conform to fixed genetic patterns.

## Motivation

This library stems from a research project that explored the limits of GA customization. In that project, we:

- Designed mutation operators that take one parent and return _multiple_ children.
- Composed several crossover and mutation strategies in a single run.
- Observed significantly improved performance when operator outputs were more likely to outperform their inputs.
- Produced best-in-class results on the N-Queens problem for large values of N.

The key insight was this: when mutation and crossover operators are designed to be domain-aware and flexible, they can produce fitter children more consistently. This led to a dramatic performance increase in solving combinatorial problems like N-Queens, TSP, and bin packing.

## Core Principles

- **Generic**: Every component — selection, crossover, mutation, merge, fitness, stop — is generic and user-pluggable.
- **Composable**: You can use multiple crossover or mutation operators within a single GA instance.
- **Type-safe**: Strong Haskell types make it hard to misuse components.
- **Functional**: No mutation, no side-effects in core logic — everything is pure.
- **Transparent**: No black-box machinery; the GA flow is clear and customizable.
- **Explicit Merge**: The framework introduces a _merge_ component to control how new individuals are integrated. Are populations simply concatenated? Are duplicates removed? This operator gives you fine-grained control over population diversity, elitism, and replacement strategies.
- **Proportional Operator Use**: Instead of applying mutation and crossover based on probability, operators are assigned **proportions** — e.g., `0.3` means 30% of the selected child population should be produced by that operator before merging. This makes the evolutionary dynamics more interpretable and tunable.

## On Repackaging Standard Methods

Yes — this framework unifies traditional operators under a shared model. That’s intentional. The goal is not to replace the foundational concepts of GAs but to empower researchers and developers to **tailor every stage of evolution to their specific problem**. Every stage can use standard defaults — or be redesigned entirely.

In this way, `GeneticAlgorithmLib` isn’t just a toolkit. It’s a sandbox for algorithmic creativity.

# Using The Framework

N Queens will be used as a concrete exaple of how to deploy the framework. When creating a solution the following components **must** be defined as are problem specific.

- **Representation**
- **Inital Population**
- **Fitness**

All other componants have pre defined standard implementaitons that can be deployed.

## Representation

Chromosomes (solutions) of the GA are defined in terms of the generic type `c`, when the user deploys the framework `c` must be defined. In this example the chromosome is Board represented as a list of ints.

```haskell
-- when n = 5 a valid representation would be
-- [0, 1, 4, 2, 3]
-- where the index is a row and the value is the column
--  x - - - -
--  - x - - -
--  - - - - x
--  - - x - -
--  - - - x -
type Board = [Int]
```

## Random Chromosome

Now that the representation has been defined the initial population must be defined. This can be a custom initial population or a random one. Here we will define a random one.

```haskell
data InitialPopulation c = RandChrom (MkRand c) | InitialisedPop (Pop c)
type MkRand c = Seed -> c
```

Here we define our function of type `MkRand` that takes a list of integers of size n for the n number of queens, a seed and uses the `Utility` function shuffle to shuffle the list.

```haskell
mkRandQueen :: Int -> MkRand Board
mkRandQueen n seed = shuffle seed [1..n]
```

## Fitness

The merit of each chromosome must be evaluated by defining a fitness function.

```haskell
type FitnessValue = Int
type Fitness c = c -> FitnessValue
```

In this example our fittness will be the number of collisions on the board are calculated. It is important to note that the fittness is ordered such that lower is better **always**.

In the example we only have to find the number of diagonal collisions due to how the problem has been represented. No two queens can ever exist in the same column and row thus the only collisions we can have are diagonal.

```haskell
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
```

## Genetic Operators

In order for the population to evolve, Crossover and Mutation can be defined. They are defined as Genetic Operators taking a list of parent chromosomes and producing a list of children.
These operators are wrapped in configs containg meta data about the operator for selection.

```haskell
type GeneticOpperator c = ChromSize -> Seed -> [c] -> [c]

data OpperatorConfig c = OpperatorConfig {
        opperator ::GeneticOpperator c, -- Operator function
        proportion :: Float,            -- Proportion of population
        numParents :: Int,              -- parents required for operator
        numChildren :: Int              -- Number of children children created by operator
    }
```

### Crossover

In the framework the following have been defined:

- **OnePoint Crossover**
- **Permutation Crossover**

### Mutation

In the framework the following have been defined:

- **Mutation by Swap** (Allele Swap)

## Selection

To select the next generation a selection method must be chosen. Selection returns an unbounded list. The list becomes bounded in the evolution function in the GA.

```haskell
type Selection c = Seed -> Pop (Eval c) -> Pop (Eval c)
```

In the framework the following have been defined:

- **Roulette Selection**
- **Tournement Selection**
- **Elite Selection**

## Merge

This operator defines the behavior of how the populations of crossover, merge and the persisting populaiton will interact. The function takes two population and merges them to create a new population.

```haskell
type Merge c = Pop (Eval c) -> Pop (Eval c) -> Pop (Eval c)
```

In the framework the following have been defined:

- **Concat Merge**
- **Ordered Merge** (removes some duplicates)
- **Distinct Ordered Merge** (removes all duplicates)

## Stop

The stop function describes when the GA should stop evolving, the GA will always stop after the required number of generations is reached.

```haskell
type Stop c = Pop (Eval c) -> Bool
```

In the framework the following have been defined:

- **Dont Stop**
- **Fitness Stop**

## GA

Here all of the components are used to create an instance of the GA. The GA returns a list of all of the populations produced by a run of the GA.

```haskell
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

geneticAlgorithm :: (Ord c) => GAConfig c -> Seed -> [Pop (Eval c)]
```

In the example of N Queens, an insatnce called gaForQueens can be created. It takes the problem specific parameters of N Queen. In this the number of parents and children are defined for both mutation and crossover.

```haskell
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
            , stop = fitnessStop 0
            }
```

## Displaying

To display the outputs to the console, a generic type has been defined such that either a user can implement their own display or use one of the pre defined displays. Display takes a population of evaluated chromsomes and a window, then returns a monad to display the contents to the console.

```haskell
type DisplayPop c = Show c => [Pop (Eval c)] -> Int -> IO ()
```

There is a pre defined function called display that returns windows of each generation, displaying first the fitness and then the chromosome. Each generation has its generation number and population size displayed.

In our example, we can call the generic display function within our main function. Here is where we define the mutation/crossover probabilities, the population size, max generations, seed and any other problem specific values needed for the GA

```haskell
main :: IO ()
main = do
  let n = 20
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.7
  let mProb = 0.15
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  display solutions 5
```

_More sections coming soon: installation, usage, examples, and extension._
