module Main where

import Racetrack (run,startRacetrack,randomHorses)

main :: IO ()
main = do
  putStrLn "And they're off!"
  putStrLn $ show $ run startRacetrack randomHorses
  
