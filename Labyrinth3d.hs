import Tools
import LocalSettings

data MazePosition = MazeOff
                  | MazeAt Int Int Char
                  | MazeSolved

draw s = mapM_ putStrLn s >> (mapM_ putStrLn $ take (screenSize - length s) (repeat ""))

move :: [String] -> Int -> Int -> Char -> Char -> MazePosition
move m x y c mv =
    let (x1,y1,c1) = case (c,mv) of
                         ('v','k') -> (x,y,'<')
                         ('<','k') -> (x,y,'^')
                         ('^','k') -> (x,y,'>')
                         ('>','k') -> (x,y,'v')
                         
                         ('v','j') -> (x,y,'>')
                         ('<','j') -> (x,y,'v')
                         ('^','j') -> (x,y,'<')
                         ('>','j') -> (x,y,'^')
                         
                         ('v','i') -> (x,y+1,c)
                         ('<','i') -> (x-1,y,c)
                         ('^','i') -> (x,y-1,c)
                         ('>','i') -> (x+1,y,c)
                         
                         ('v','m') -> (x,y-1,c)
                         ('<','m') -> (x+1,y,c)
                         ('^','m') -> (x,y+1,c)
                         ('>','m') -> (x-1,y,c)
                         _ -> (-1,-1,c)
    in
        if (y1 >= length m) then MazeSolved
        else if or [x1 < 0, y1 < 0, x1 >= length (m !! y1), ' ' /= m !! y1 !! x1] then MazeOff                             
        else MazeAt x1 y1 c1
        
mainLoop :: [String] -> Int -> Int -> Char -> IO ()
mainLoop m x y c = do
    mv <- getChar
    if mv == 'x' then
        return ()
    else
        case move m x y c mv of
            MazeOff -> mainLoop m x y c
            MazeAt x1 y1 c1 -> do
                draw $ updateMap c1 m x1 y1
                mainLoop m x1 y1 c1
            MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"


main :: IO ()
main = do
    mazeString <- readFile "Labyrinth3d.map"
    let maze = lines mazeString
    draw $ updateMap 'v' maze 0 0
    mainLoop maze 0 0 'v'
