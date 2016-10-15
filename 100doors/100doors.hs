module Main (main) where

main :: IO ()
main = mapM_ printDoor (play 1 (createGame 100))

play :: Int -> [Door] -> [Door]
play n x
  | n > length x = x
  | otherwise = 
      play
        (n+1)
        (map (\d -> if (fst d) `mod` n == 0
                    then ((fst d), (not (snd d)))
                    else d)
          x)

createGame :: Int -> [Door]
createGame 0 = []
createGame n = reverse (createGameRev n)
  where createGameRev n
          | n == 0 = []
          | otherwise = (n, False):(createGameRev (n-1))

type Door = (Int, Bool)

printDoor :: Door -> IO ()
printDoor = (putStrLn . doorToString)

doorToString :: Door -> String
doorToString a = concat [
    show (fst a),
    ". ",
    if (snd a)
      then "open"
      else "closed"
  ]
