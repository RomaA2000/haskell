{-# LANGUAGE ScopedTypeVariables #-}
{-
Module Task8:
Function for simulation.
-}
module Task8
  ( -- * The 'simulate' implementation.
    simulate
  ) where

import System.Console.ANSI
import Control.Comonad
import Control.Monad.Reader
import System.Random (mkStdGen, StdGen, random, randoms)

-- | Status type.
data Status
  -- | Infected.
  = Inf Int
  -- | Immunity.
  | Imm Int
  -- | Symptoms.
  | Sym Int
  deriving (Eq, Show)

-- | Person type.
data Person = Person
  { -- | Random generator.
    rand :: StdGen
    -- | Status of person.
  , status :: Status
  } deriving (Show)

-- | Params type.
data Params = Params
  { -- | seed for random.
    seed :: Int
    -- | probability of infecting.
  , p :: Double
    -- | incubation period in steps.
  , inc :: Int
    -- | immunity period in steps.
  , imm :: Int
    -- | symptomatic period in steps.
  , sym :: Int
    -- | size of grid in cmd.
  , size :: Int
  } deriving (Eq, Show, Read)

-- | Simulation type for task.
type Simulation = Grid Person

-- | Converts person status to char.
out :: Status -> Char
out (Sym _) = '#'
out (Imm 0) = ' '
out (Inf _) = 'o'
out _ = '@'

-- | Reads parameters from cmd.
getParams :: IO Params
getParams = do
  putStrLn "Random seed:"
  (_seed :: Int) <- readLn
  putStrLn "Probability:"
  (_p :: Double) <- readLn
  putStrLn "Incubation length:"
  (_inc :: Int) <- readLn
  putStrLn "Symptomatic length:"
  (_sym :: Int) <- readLn
  putStrLn "Immunity length:"
  (_imm :: Int) <- readLn
  putStrLn "Grid size:"
  (_size :: Int) <- readLn
  return $ Params _seed _p _inc _sym _imm _size

-- | Simulates one step.
simulateStep :: Simulation -> ReaderT Params IO ()
simulateStep model = do
  cmd <- lift $ getLine
  case cmd of
    "" -> do
      lift $ clearScreen
      printGrid model
      params <- ask
      let newModel = runReader (step model) params
      simulateStep newModel
    "exit" -> return ()
    _ -> do
      lift $ putStrLn "unknown command"
      simulateStep model

-- | Gets parameters and simulates.
simulator :: ReaderT Params IO ()
simulator = do
  params <- ask
  let vals = runReader firstPatient params
  simulateStep vals

-- | Reads parameters and starts simulation.
simulate :: IO ()
simulate = do
  config <- getParams
  runReaderT simulator config

-- | Reads parameters and returns infected person.
infectedNow :: Reader Params Person
infectedNow = do
  params <- ask
  return $ Person (mkStdGen $ seed params) (Inf $ inc params)

-- | Returns first infected person.
firstPatient :: Reader Params Simulation
firstPatient = do
  params <- ask
  now <- infectedNow
  let s = seed params
  let randomLZ = LZ (randoms $ mkStdGen s) 0 (randoms $ mkStdGen (- s))
  let randomGrid = Grid (iterateF (fmap (* (-3))) (fmap (* (-5))) randomLZ)
  let sim = (flip Person) (Imm 0) . mkStdGen <$> randomGrid
  return $ replace now sim

-- | Tries to infect person.
tryInfect :: Int -> Person -> Reader Params Person
tryInfect num person = do
  params <- ask
  let iter = iterate (random . snd) (1.0, rand person)
  let generatorList = take (1 + num) iter
  case any ((< p params) . fst) generatorList of
    True -> do
      now <- infectedNow
      return $ (now) {rand = snd $ last generatorList}
    False -> return $ Person (snd $ last generatorList) (Imm 0)

-- | Counts infected persons.
infN :: Simulation -> Int
infN g = length $ filter isInf $ map (\v -> extract $ v g) [up, right, down, left]

-- | Updates simulation.
updateGrid :: Params -> Simulation -> Person
updateGrid params model = do
  let patient = extract model
  case status patient of
    (Imm x)
      | x > 1 -> patient {status = Imm $ x - 1}
      | otherwise -> runReader (tryInfect (infN model) patient) params
    (Sym x)
      | x > 1 -> patient {status = Sym $ x - 1}
      | otherwise -> patient {status = Imm $ imm params}
    (Inf x)
      | x > 1 -> patient {status = Inf $ x - 1}
      | otherwise -> patient {status = Sym $ sym params}

-- | Prints grid from simulation.
printGrid :: Simulation -> ReaderT Params IO ()
printGrid model = do
  params <- ask
  let sz = size params
  let grid = cutGrid sz $ out . status <$> model
  mapM_ (liftIO . putStrLn) grid

-- | Prints one step of simulation.
step :: Simulation -> Reader Params Simulation
step val = do
  params <- ask
  return $ extend (updateGrid params) val

-- | Checks if person is infected.
isInf :: Person -> Bool
isInf (Person _ (Imm _)) = False
isInf _ = True

-- | Definition for ListZipper.
data ListZipper v = LZ [v] v [v]

-- | Moves LZ left.
mL :: ListZipper v -> ListZipper v
mL (LZ (l:ls) m rs) = LZ ls l (m:rs)
mL (LZ [] _ _) = error "error in mL"

-- | Moves LZ right.
mR :: ListZipper v -> ListZipper v
mR (LZ ls m (r:rs)) = LZ (m:ls) r rs
mR (LZ _ _ []) = error "error in mR"

-- | Iterates in LZ with function.
iterateF :: (v -> v) -> (v -> v) -> v -> ListZipper v
iterateF f g x = LZ (tail $ iterate f x) x (tail $ iterate g x)

-- | Swaps center element of LZ.
centerSwap :: v -> ListZipper v -> ListZipper v
centerSwap val (LZ ls _ rs) = (LZ ls val rs)

-- | Cuts center element of LZ.
cutZipper :: Int -> ListZipper v -> [v]
cutZipper sz (LZ ls x rs) = reverse (take sz ls) ++ ([x] ++ take sz rs)

-- | Functor instance for LZ.
instance Functor ListZipper where
  fmap :: (f -> s) -> ListZipper f -> ListZipper s
  fmap func (LZ ls e rs) = LZ (map func ls) (func e) (map func rs)

-- | Comonad instance for LZ.
instance Comonad ListZipper where
  extract :: ListZipper v -> v
  extract (LZ _ e _) = e

  duplicate :: ListZipper v -> ListZipper (ListZipper v)
  duplicate = iterateF mL mR

-- | Definition for Grid.
newtype Grid v = Grid (ListZipper (ListZipper v))

-- | Functor instance for Grid.
instance Functor Grid where
  fmap :: (f -> s) -> Grid f -> Grid s
  fmap f (Grid grid) = Grid $ fmap f <$> grid

-- | Comonad instance for Grid.
instance Comonad Grid where
  extract :: Grid v -> v
  extract (Grid grid) = extract $ extract grid

  duplicate :: Grid v -> Grid (Grid v)
  duplicate = Grid . fmap horizontal . vertical

-- | Horizontal move for Grid to LZ.
horizontal :: Grid v -> ListZipper (Grid v)
horizontal = iterateF left right

-- | Vertical move for Grid to LZ.
vertical :: Grid v -> ListZipper (Grid v)
vertical = iterateF up down

-- | Up move for Grid.
up :: Grid v -> Grid v
up (Grid g) = Grid $ mL g

-- | Down move for Grid.
down :: Grid v -> Grid v
down (Grid g) = Grid $ mR g

-- | Left move for Grid.
left :: Grid v -> Grid v
left (Grid g) = Grid $ fmap mL g

-- | Right move for Grid.
right :: Grid v -> Grid v
right (Grid g) = Grid $ fmap mR g

-- | Replaces center element of Grid.
replace :: v -> Grid v -> Grid v
replace val (Grid g) = Grid $ centerSwap (centerSwap val (extract g)) g

-- | Cuts grid to [[]].
cutGrid :: Int -> Grid v -> [[v]]
cutGrid sz (Grid g) = cutZipper sz $ cutZipper sz <$> g