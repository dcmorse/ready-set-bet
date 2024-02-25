module Main where

import Racetrack

main :: IO ()
main = do
  putStrLn "FROM START LINE"
  putStr $ fmtTable $ table 1000 startRacetrack
  putStrLn "\n\n"
  putStrLn "AFTER DOUBLE 5s"
  putStr $ fmtTable $ table 1000 $ moveHorse (Horse 5) $ moveHorse (Horse 5) startRacetrack
  
