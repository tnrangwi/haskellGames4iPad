import Tools
import LocalSettings
import Text.Printf
import Data.List

data MovePosition = MoveOff
                  | MoveSwap Int Int
                  | MoveSolved

draw :: [[String]] -> IO ()
draw m = mapM_ (putStrLn . concat) m >> (mapM_ putStrLn $ take (screenSize - length m) (repeat ""))

move :: [[String]] -> Int -> Int -> Char -> Maybe (Int,Int)
move m x y c = let (x1,y1) = case c of
                                 'i' -> (x,y + 1)
                                 'm' -> (x,y - 1)
                                 'j' -> (x + 1,y)
                                 'k' -> (x - 1,y)
                                 _ -> (-1,-1)
                           
               in
                   if or [x1<0,y1<0,y1>=(length m),x1>=(length  $ m!!y1)] then
                       Nothing
                   else
                       Just (x1,y1)

roll :: [[String]] -> (Int,Int) -> (Int,Int) -> [[String]]
roll m (x,y) (x1,y1) = let s = m !! y !! x
                           s1 = m !! y1 !! x1
                           u1 = updateAt m y (updateAt (m !! y) x s1)
                       in
                           updateAt u1 y1 (updateAt (u1 !! y1) x1 s)

finalForm :: [[String]] -> Bool
finalForm m = let fl = concat m
                  l = length fl
                  o (x:y:ys) = and [x<y,o (y:ys)]
                  o _ = True
              in
                  and [" .. " == fl !! (l-1),o (take (l - 1) fl)]
getStart :: [Int] -> (Int,Int)
getStart m = let i = elemIndex 1 m
             in
                 case i of
                     Just p -> (p `rem` 4, p `quot` 4)
                     Nothing -> error "Can't find start"

getNumberMap :: [Int] -> [[String]]
getNumberMap l = let sp s = " " ++ s ++ " "
                     m 0 = sp ".."
                     m i = sp $ printf "%02d" i
                     chunk [] = []
                     chunk l = let
                                   (b,e) = splitAt 4 l
                               in
                                    [b] ++ (chunk e)
                 in chunk $ map m l


mainLoop :: [[String]] -> Int -> Int -> IO ()
mainLoop m x y = do
    mv <- getChar
    if mv == 'x' then
        return ()
    else
        case move m x y mv of
            Nothing -> mainLoop m x y
            Just (x1,y1) -> do
                let m1 = roll m (x,y) (x1,y1)
                draw m1
                if finalForm m1 then
                    putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"
                else
                    mainLoop m1 x1 y1


main :: IO ()
main = do
    box <- getRandomSequence 16
    let gamesMap = getNumberMap $ map pred box
    draw gamesMap
    let (x,y) = getStart box
    mainLoop gamesMap x y
