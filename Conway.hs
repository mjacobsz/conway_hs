module Conway where

import Data.List (intercalate)

type Cell   = (Int,Int)


putGrid :: [Cell] -> IO ()
putGrid = putStrLn . toString

next :: [Cell] -> [Cell]
next cs = cs'
    where
      cs' = [(x,y) | x <- [(left cs)..(right cs)]
                   , y <- [(top cs)..(bottom cs)]
                   , ((x,y) `elem` cs && count (x,y) == 2)
                     || count (x,y)  == 3]
      count (x,y) = length (filter (\c -> c `elem` cs)
                                   [(x+x',y+y') | x' <- [-1..1]
                                                , y' <- [-1..1]
                                                , x' /= 0 || y' /= 0])

toString :: [Cell] -> String
toString cs =
    intercalate "\n"
                (map (\y -> concatMap (\x -> render(x,y)) xs) ys)
    where
      xs = [(left cs)..(right cs)]
      ys = [(top cs)..(bottom cs)]
      render (x,y) = if (x,y) `elem` cs then "#" else "."

left   cs = minimum (map fst cs) - 1
right  cs = maximum (map fst cs) + 1
top    cs = minimum (map snd cs) - 1
bottom cs = maximum (map snd cs) + 1
