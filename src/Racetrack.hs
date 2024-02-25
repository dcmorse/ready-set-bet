module Racetrack where

import System.Random
import Control.Monad
import qualified Data.Vector as Vector
import Data.Vector ((!))
import qualified Data.Map as Map



-- Horse ________________________________________________________________


newtype Horse = Horse Int deriving (Eq, Ord)

horseId (Horse x) = x

horses :: [Horse]
horses = map Horse [3..11]

instance Show Horse where
  show (Horse 3) = "(Horse 2/3)"
  show (Horse 11) = "(Horse 11/12)"
  show (Horse x) | 4 <= x && x <= 10 = "(Horse " <> show x <> ")"


horseRampageBonus :: Horse -> Int
horseRampageBonus h =
  case horseId h of
    3 -> 3
    4 -> 3
    5 -> 2
    6 -> 1
    7 -> 0
    8 -> 1
    9 -> 2
    10 -> 3
    11 -> 3

findHorseById :: Int -> Horse
findHorseById id = (findHorseV ! (id - 2))
  where
    findHorseV = Vector.fromList [horses !! (numberNudge i - 3) | i <- [2..12]]
    numberNudge i = case i of
                      2 -> 3
                      12 -> 11
                      n | 2 < n && n < 12 -> i

-- randomHorses ________________________________________________________________

randomHorses :: [Horse]
randomHorses = map findHorseById random2D6s
  

random2D6s :: [Int]
random2D6s = f randomD6s
  where f (a:b:rest) = a + b : f rest

randomD6s :: [Int]
randomD6s = randomRs (1, 6) $ mkStdGen 4242

-- RaceTrack ________________________________________________________________

data Racetrack = Racetrack { progress :: (Map.Map Horse Int), lastMoved :: (Maybe Horse) } deriving (Show, Eq)

startRacetrack = Racetrack horsesAt0 Nothing
  where
    horsesAt0 = Map.fromList (map (\h->(h,0)) horses)

moveHorse :: Horse -> Racetrack -> Racetrack 
moveHorse horse (Racetrack progress lastMoved) = Racetrack progress' (Just horse)
  where
    progress' = Map.adjust (+ distance) horse progress
    distance = 1 + (if (Just horse) == lastMoved then horseRampageBonus horse else 0)

raceIsOver :: Racetrack -> Bool
raceIsOver racetrack = any (>= 15) $ Map.elems $ progress racetrack

run racetrack (h:hs) =
  if raceIsOver racetrack
  then racetrack
  else run (moveHorse h racetrack) hs
