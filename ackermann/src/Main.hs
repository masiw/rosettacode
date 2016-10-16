module Main (main) where

import Paths_ackermann (version)
import Data.Version (showVersion)
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs args = case args of
          x:y:xs -> do
            print (ackermann (read x :: Integer) (read y :: Integer))
            exit
          _ -> do
            useage
            exit
        
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
  
useage :: IO ()
useage = do
  putStrLn "type ackermann integer integer to calculate the ackermann number"

exit :: IO ()
exit = exitWith ExitSuccess
