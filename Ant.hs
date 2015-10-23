import Data.Matrix
import Control.Concurrent (threadDelay)
import System.Random
import Control.DeepSeq
import Data.List (foldl')

type Position = (Int, Int)

data FoodStatus = HasFood !FoodPheromone | Searching !NestPheromone deriving Show

data Ant = Ant !FoodStatus !Position !StdGen deriving Show

type NestPheromone = Int
type FoodPheromone = Int

type FoodSize = Int

data Terrain = Empty !NestPheromone !FoodPheromone | Food !FoodSize | Obstacle deriving Show

instance NFData FoodStatus where
  rnf (HasFood p) = seq p ()
  rnf (Searching p) = seq p ()

instance NFData Ant where --FIXME stdgen
  rnf (Ant f p s) = deepseq f (deepseq p (deepseq p ()))

instance NFData Terrain where
  rnf Obstacle = ()
  rnf (Food x) = rnf x
  rnf (Empty a b) = seq a (seq b ())

isEmpty :: Terrain -> Bool
isEmpty (Empty _ _) = True
isEmpty _ = False

isFood :: Terrain -> Bool
isFood (Food _) = True
isFood _ = False

isObstacle :: Terrain -> Bool
isObstacle Obstacle = True
isObstacle _ = False

dimx = 10
dimy = 10
nestPos = (2,2)
nants = 300
initialNestPheromone = 20
initialFoodPheromone = 20
maxNestPheromone = 1000
maxFoodPheromone = 1000
nestPheromoneEvaporation = 3
foodPheromoneEvaporation = 3
nestForgettingRate = 1
foodForgettingRate = 1
defActractiveness = 5

defTerrain = setElem (Food 10000) (10,10) $ matrix dimx dimy $ const $ Empty 0 0

defAnt = Ant (Searching initialNestPheromone) nestPos

calcWeight :: Matrix Terrain -> FoodStatus -> Position -> Int
calcWeight terrain status pos = actractiveness (terrain!pos) status --TODO merge

actractiveness :: Terrain -> FoodStatus -> Int
actractiveness Obstacle _ = 0
actractiveness (Food n) (Searching _) = n*1000 + defActractiveness
actractiveness (Empty nestPheromone _) (HasFood _) = nestPheromone + defActractiveness
actractiveness (Empty _ foodPheromone) (Searching _) = foodPheromone + defActractiveness
actractiveness _ _ = defActractiveness

weightedRandom :: StdGen -> [(Int, a)] -> (StdGen, a)
weightedRandom rand xs = (rand', x)
  where (n, rand') = randomR (0, (sum $ fmap fst xs)-1) rand
        x = xs `weightedIndex` n

weightedIndex :: [(Int, a)] -> Int -> a
weightedIndex ((w, x):xs) i | i>w = xs `weightedIndex` (i-w)
                            | otherwise = x
weightedIndex _ i | i<0 = error "Negative index on weightedIndex"
weightedIndex [] _ = error "Index too large on weightedIndex"

inBounds :: (Num a, Num b, Ord a, Ord b) => a -> b -> (a, b) -> Bool
inBounds mx my (x, y) | x>0 && y>0 && x<=mx && y<=my = True
                      | otherwise = False

moveAnt :: Matrix Terrain -> Ant -> Ant
moveAnt terrain (Ant antType (x, y) rand) = Ant antType pos rand'
 where (rand', pos) = weightedRandom rand
                   $ fmap (\x -> (calcWeight terrain antType x, x))
                   $ filter (inBounds dimx dimy)
                   [ (x+1, y)
                   , (x-1, y)
                   , (x, y+1)
                   , (x, y-1)
                   , (x+1, y+1)
                   , (x-1, y-1)
                   , (x+1, y-1)
                   , (x-1, y+1)
                   ]

forgetNest :: Ant -> Ant
forgetNest (Ant (Searching nestPheromone) pos rand) = Ant (Searching $ max 0 $ nestPheromone - nestForgettingRate) pos rand
forgetNest (Ant (HasFood   foodPheromone) pos rand) = Ant (HasFood $ max 0 $ foodPheromone - foodForgettingRate) pos rand

placePheromone :: Ant -> Matrix Terrain -> Matrix Terrain
placePheromone (Ant (HasFood   pheromone) pos _) terrain | isEmpty (terrain!pos) =
  setElem (Empty nestPheromone (min maxFoodPheromone $ foodPheromone + pheromone)) pos terrain
    where (Empty nestPheromone foodPheromone) = terrain!pos
placePheromone (Ant (Searching pheromone) pos _) terrain | isEmpty (terrain!pos) =
  setElem (Empty (min maxNestPheromone $ nestPheromone + pheromone) foodPheromone) pos terrain
    where (Empty nestPheromone foodPheromone) = terrain!pos
placePheromone _ x = x

evaporatePheromone :: Terrain -> Terrain --TODO exponential or something
evaporatePheromone (Empty nestPheromone foodPheromone) = Empty (max 0 $ nestPheromone * 19 `div` 20) -- - nestPheromoneEvaporation)
                                                               (max 0 $ foodPheromone * 19 `div` 20) -- - foodPheromoneEvaporation)
evaporatePheromone x = x

takeFood :: Ant -> (Matrix Terrain, [Ant]) -> (Matrix Terrain, [Ant])
takeFood (Ant (Searching _) pos rand) (terrain, ants) | isFood (terrain!pos) =
  (setElem t' pos terrain, ant':ants)
    where Food food = terrain!pos --safe
          t' | food-1 > 0 = Food $ food-1
             | otherwise = Empty 0 maxFoodPheromone
          ant' = Ant (HasFood initialFoodPheromone) pos rand
takeFood a (t, as) = (t, a:as)

placeFood :: Ant -> (Matrix Terrain, [Ant]) -> (Matrix Terrain, [Ant])
placeFood (Ant (HasFood _) pos rand) (terrain, ants) | pos==nestPos =
  (terrain, ant':ants)
    where ant' = Ant (Searching initialNestPheromone) pos rand
placeFood a (t, as) = (t, a:as)

simulate :: (Matrix Terrain, [Ant]) -> (Matrix Terrain, [Ant])
simulate = id
         . force --avoid memory leaks
         . mapFst (fmap evaporatePheromone)
         . (\(t, as) -> (foldl' (flip placePheromone) t as, as))
         . mapSnd (fmap forgetNest)
         . (\(t, as) -> (t, fmap (moveAnt t) as))
         . (\(t, as) -> foldl' (flip placeFood) (t, []) as)
         . (\(t, as) -> foldl' (flip takeFood)  (t, []) as)

printAll :: Matrix Terrain -> [Ant] -> IO ()
printAll terrain ants = do
  let antMatrix = foldr (\(Ant _ pos _) m -> setElem 0 pos m) (matrix dimx dimy $ const 1) ants
  putStrLn $ prettyMatrix antMatrix
  --putStrLn $ prettyMatrix terrain
  --putStrLn $ show ants
  return ()

simulateIO :: Matrix Terrain -> [Ant] -> IO ()
simulateIO terrain ants = do
  let (terrain', ants') = simulate (terrain, ants)
  printAll terrain' ants'
  threadDelay 100000
  simulateIO terrain' ants'

main :: IO ()
main = do
  putStrLn "Simulating"
  let terrain = defTerrain
  gen <- getStdGen
  let rands = map mkStdGen $ randoms gen
  let ants = take nants $ map defAnt rands
  simulateIO terrain ants
  return ()

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

