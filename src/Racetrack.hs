module Racetrack where

import System.Random
import Control.Monad
import qualified Data.Vector as Vector
import Data.Vector ((!))
import qualified Data.Map as Map
import Text.Printf



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
moveHorse horse (Racetrack progress lastMoved) = Racetrack progress' rampageHorse
  where
    rampageHorse = if justHorse == lastMoved then Nothing else justHorse
    progress' = Map.adjust (+ distance) horse progress
    distance = 1 + (if justHorse == lastMoved then horseRampageBonus horse else 0)
    justHorse = (Just horse)
    

raceIsOver :: Racetrack -> Bool
raceIsOver racetrack = any (>= 15) $ Map.elems $ progress racetrack

run :: Racetrack -> [Horse] -> Racetrack
run racetrack = fst . run' racetrack

run' :: Racetrack -> [Horse] -> (Racetrack,[Horse])
run' racetrack horses@(h:hs) =
  if raceIsOver racetrack
  then (racetrack, horses)
  else run' (moveHorse h racetrack) hs

runTimes' :: Racetrack -> [Horse] -> Int -> ([Racetrack],[Horse])
runTimes' track horses 0 = ([],horses)
runTimes' track horses n =
  let (tracksT, horsesT) = runTimes' track horses (n-1)
      (track', horses') = run' track horsesT
  in (track':tracksT, horses')

runTimes :: Racetrack -> [Horse] -> Int -> [Racetrack]
runTimes racetrack horses = fst . runTimes' racetrack horses


-- pays :: Horse -> Racetrack -> Int -> Bool


type TrackOrdering = [[Horse]]
trackOrdering :: Racetrack -> [[Horse]]
trackOrdering (Racetrack progress _) =
  -- in [(Int, [Horse])], 
  -- Int is the distance those horses have travelled, [Horse]s are the horses at that position. List is sorted descending distance.
  map snd $ Map.foldrWithKey f [] progress 
  where
    f :: Horse -> Int -> [(Int, [Horse])] -> [(Int, [Horse])]
    f horse x [] = [(x, [horse])]
    f horse x all@((acc@(x',horses)):accs) | x > x' = (x, [horse]) : all
                                           | x == x' = (x, horse:horses) : accs
                                           | otherwise = acc : f horse x accs

inTop :: Int -> Horse -> TrackOrdering -> Bool
inTop n horse (first:rest) | n <= 0 = False
                           | elem horse first = True
                           | otherwise = inTop (n - length first) horse rest
                           
pInTop :: Int -> Horse -> [TrackOrdering] -> Double
pInTop n horse trackOrderings =
  fromIntegral (length $ filter (inTop n horse) trackOrderings) /
  fromIntegral (length trackOrderings)

-- pInTop 1 (Horse 7) $ map trackOrdering $ runTimes startRacetrack randomHorses 1000
data HorseOdds = HorseOdds Horse Double Double Double deriving Show
-- { horse :: Horse, win :: Double, place :: Double, show :: Double } deriving Show
tableRow (HorseOdds h w p s) =
  printf "%13s %0.3f %0.3f %0.3f" (show (h :: Horse)) w p s :: String
tableHeader =  "               win  place show "

-- mapM_ (print . tableRow) $ table 1000 startRacetrack

table :: Int -> Racetrack -> [HorseOdds]
table trials racetrack =
  map odds horses
  where
    odds h = HorseOdds h (pInTop 1 h tos) (pInTop 2 h tos) (pInTop 3 h tos)
    tos = map trackOrdering $ runTimes racetrack randomHorses trials

fmtTable table = unlines $ tableHeader : (map tableRow table)
