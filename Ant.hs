import Control.Concurrent (threadDelay)
import System.Random
import Control.DeepSeq
import Data.List (foldl', minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Monoid
import Data.Function (on)

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

isHasFood (HasFood _) = True
isHasFood _ = False

fps = 40
cellDimI = 16
cellDim = fromIntegral cellDimI
dimx = 20
dimy = 20
nestPos = (2,2)
nants = 100
initialNestPheromone = 200
initialFoodPheromone = 200
maxNestPheromone = 1000
maxFoodPheromone = 1000
nestPheromoneEvaporation = 3
foodPheromoneEvaporation = 3
nestForgettingRate = 1
foodForgettingRate = 1
defActractiveness = 50

--defTerrain = insert (18, 18) (Food 100) $ empty
defTerrain = foldr (flip Map.insert (Food 100)) Map.empty [(18, 18), (17, 18), (15, 19), (19, 5), (2, 2), (20, 20)]

defAnt = Ant (Searching initialNestPheromone) nestPos

inverseDistance :: Position -> Position -> Int
inverseDistance (x1',y1') (x2',y2') = floor $ sqrt ((x1-x2)^2+(y1-y2)^2)
  where x1 = fromIntegral x1'
        y1 = fromIntegral y1'
        x2 = fromIntegral x2'
        y2 = fromIntegral y2'

nearest xs = fst $ minimumBy (compare `on` snd) $ zip xs (map (inverseDistance nestPos) xs)

lookupTerrain k m = fromMaybe (Empty 0 0) $ Map.lookup k m

calcWeight :: Map Position Terrain -> FoodStatus -> Position -> Int
calcWeight terrain status pos = actractiveness pos (lookupTerrain pos terrain) status --TODO merge

actractiveness :: Position -> Terrain -> FoodStatus -> Int
actractiveness _ Obstacle _ = 0
actractiveness _ (Food n) (Searching _) = n*1000 + defActractiveness
actractiveness pos (Empty nestPheromone _) (HasFood _) = inverseDistance pos nestPos
actractiveness _ (Empty _ foodPheromone) (Searching _) = foodPheromone + defActractiveness
actractiveness _ _ _ = defActractiveness

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
moveAnt terrain (Ant antType (x, y) rand) = Ant antType pos'' rand''
 where (rand'',pos'') = if isHasFood antType then (rand, nearest [ (x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]) else (rand',pos)
       (rand', pos) = weightedRandom rand
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
  Map.insert pos (Empty nestPheromone (min maxFoodPheromone $ foodPheromone + pheromone)) terrain
    where (Empty nestPheromone foodPheromone) = lookupTerrain pos terrain
placePheromone (Ant (Searching pheromone) pos _) terrain | isEmpty (lookupTerrain pos terrain) =
  Map.insert pos (Empty (min maxNestPheromone $ nestPheromone + pheromone) foodPheromone) terrain
    where (Empty nestPheromone foodPheromone) = lookupTerrain pos terrain
placePheromone _ x = x

evaporatePheromone :: Terrain -> Terrain --TODO exponential or something
evaporatePheromone (Empty nestPheromone foodPheromone) = Empty (max 0 $ (nestPheromone * 60 `div` 61) -nestPheromoneEvaporation) -- - nestPheromoneEvaporation)
                                                               (max 0 $ (foodPheromone * 60 `div` 61) -foodPheromoneEvaporation) -- - foodPheromoneEvaporation)
evaporatePheromone x = x

takeFood :: Ant -> (Map Position Terrain, [Ant]) -> (Map Position Terrain, [Ant])
takeFood (Ant (Searching _) pos rand) (terrain, ants) | isFood (lookupTerrain pos terrain) =
  (Map.insert pos t' terrain, ant':ants)
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
  let antMatrix = foldr (\(Ant _ pos _) m -> Map.insert pos 0 m) (Map.empty) ants
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
  where terrainP = Pictures $ fmap (uncurry terrain2pic) $ Map.toList terrain
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

