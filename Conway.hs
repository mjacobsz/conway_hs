module Conway where

type Width  = Int
type Height = Int
type Cell   = (Int,Int)
type Grid   = (Width,Height,[Cell])

nbCount :: Cell -> [Cell] -> Int
nbCount (x,y) cs =
    length (filter (\c -> c `elem` cs) 
                   [(x+x',y+y') | x' <- [-1..1] 
                                , y' <- [-1..1]
                                , x' /= 0 || y' /= 0])

next :: Grid -> Grid
next (w,h,cs) = (w,h,cs')
    where cs' = [(x,y) | x <- [1..w]
                       , y <- [1..h]
                       , ((x,y) `elem` cs && nbCount (x,y) cs == 2)
                         || nbCount (x,y) cs == 3]
