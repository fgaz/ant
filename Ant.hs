import Control.Concurrent (threadDelay)
import System.Random
import Control.DeepSeq
import Data.List (foldl')
import Prelude hiding (lookup)
import Data.Map hiding ((!), map, foldl, foldl', foldr, filter)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Monoid

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

fps = 60
cellDimI = 16
cellDim = fromIntegral cellDimI
dimx = 100
dimy = 100
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

defTerrain = insert (90, 90) (Food 10000) $ empty

defAnt = Ant (Searching initialNestPheromone) nestPos

lookupTerrain k m = fromMaybe (Empty 0 0) $ lookup k m

calcWeight :: Map Position Terrain -> FoodStatus -> Position -> Int
calcWeight terrain status pos = actractiveness (lookupTerrain pos terrain) status --TODO merge

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

moveAnt :: Map Position Terrain -> Ant -> Ant
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

placePheromone :: Ant -> Map Position Terrain -> Map Position Terrain
placePheromone (Ant (HasFood   pheromone) pos _) terrain | isEmpty (lookupTerrain pos terrain) =
  insert pos (Empty nestPheromone (min maxFoodPheromone $ foodPheromone + pheromone)) terrain
    where (Empty nestPheromone foodPheromone) = lookupTerrain pos terrain
placePheromone (Ant (Searching pheromone) pos _) terrain | isEmpty (lookupTerrain pos terrain) =
  insert pos (Empty (min maxNestPheromone $ nestPheromone + pheromone) foodPheromone) terrain
    where (Empty nestPheromone foodPheromone) = lookupTerrain pos terrain
placePheromone _ x = x

evaporatePheromone :: Terrain -> Terrain --TODO exponential or something
evaporatePheromone (Empty nestPheromone foodPheromone) = Empty (max 0 $ (nestPheromone * 60 `div` 61) -nestPheromoneEvaporation) -- - nestPheromoneEvaporation)
                                                               (max 0 $ (foodPheromone * 60 `div` 61) -foodPheromoneEvaporation) -- - foodPheromoneEvaporation)
evaporatePheromone x = x

takeFood :: Ant -> (Map Position Terrain, [Ant]) -> (Map Position Terrain, [Ant])
takeFood (Ant (Searching _) pos rand) (terrain, ants) | isFood (lookupTerrain pos terrain) =
  (insert pos t' terrain, ant':ants)
    where Food food = lookupTerrain pos terrain --safe
          t' | food-1 > 0 = Food $ food-1
             | otherwise = Empty 0 maxFoodPheromone
          ant' = Ant (HasFood initialFoodPheromone) pos rand
takeFood a (t, as) = (t, a:as)

placeFood :: Ant -> (Map Position Terrain, [Ant]) -> (Map Position Terrain, [Ant])
placeFood (Ant (HasFood _) pos rand) (terrain, ants) | pos==nestPos =
  (terrain, ant':ants)
    where ant' = Ant (Searching initialNestPheromone) pos rand
placeFood a (t, as) = (t, a:as)

step :: a -> b -> (Map Position Terrain, [Ant]) -> (Map Position Terrain, [Ant])
step _ _ = id
         . force --avoid memory leaks
         . mapFst (fmap evaporatePheromone)
         . (\(t, as) -> (foldl' (flip placePheromone) t as, as))
         . mapSnd (fmap forgetNest)
         . (\(t, as) -> (t, fmap (moveAnt t) as))
         . (\(t, as) -> foldl' (flip placeFood) (t, []) as)
         . (\(t, as) -> foldl' (flip takeFood)  (t, []) as)

printAll :: Map Position Terrain -> [Ant] -> IO ()
printAll terrain ants = do
  let antMatrix = foldr (\(Ant _ pos _) m -> insert pos 0 m) (empty) ants
  putStrLn $ show antMatrix
  --putStrLn $ prettyMatrix terrain
  --putStrLn $ show ants
  return ()

{-simulateIO :: Map Position Terrain -> [Ant] -> IO ()
simulateIO terrain ants = do
  let (terrain', ants') = simulate (terrain, ants)
  printAll terrain' ants'
  threadDelay 100000
  simulateIO terrain' ants'-}

draw :: (Map Position Terrain, [Ant]) -> Picture
draw (terrain, ants) = terrainP <> nestP <> antsP
  where terrainP = Pictures $ fmap (uncurry terrain2pic) $ toList terrain
        antsP = Pictures $ fmap ant2pic ants
        nestP = Translate (zoom $ fst nestPos) (zoom $ snd nestPos) $ Color white $ rectangleSolid cellDim cellDim

terrain2pic :: Position -> Terrain -> Picture
terrain2pic (x,y) Obstacle = Translate (zoom x) (zoom y) $ Color (greyN 0.5) $ rectangleSolid cellDim cellDim
terrain2pic (x,y) (Food _) = Translate (zoom x) (zoom y) $ Color yellow $ rectangleSolid cellDim cellDim
terrain2pic (x,y) (Empty fn ff) = Translate (zoom x) (zoom y) $
                                    Color (makeColorI (ff*255 `div` maxFoodPheromone) 0 (fn*255 `div` maxNestPheromone) 255) $
                                      rectangleSolid cellDim cellDim

zoom x = fromIntegral x * cellDim

ant2pic :: Ant -> Picture
ant2pic (Ant _ (x,y) _) = Translate x' y' $ Color green $ Circle (cellDim / 2 -1)
  where x' = fromIntegral x * cellDim
        y' = fromIntegral y * cellDim

main :: IO ()
main = do
  putStrLn "Simulating"
  let terrain = defTerrain
  gen <- getStdGen
  let rands = map mkStdGen $ randoms gen
  let ants = take nants $ map defAnt rands
  simulate (InWindow "Ant" (cellDimI * fromIntegral dimx, cellDimI * dimy) (0,0))
           black
           fps
           (terrain, ants)
           draw
           step
  --simulateIO terrain ants
  return ()

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

