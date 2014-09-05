import Tools
import LocalSettings

data MazePosition = MazeOff
                  | MazeAt Int Int
                  | MazeSolved

snake = '~'

labyrinth :: [String]
labyrinth = ["  *******",
             "*  *****",
             "** *****",
             "**    **",
             "***** **",
             "*      **",
             "* *******",
             "* *******"]

labyrinth2 = ["  #######",
              "#     <- ",
              "  ###  ##",
              "#       #",
              "  #######", 
              "       <-",
              "~~~~~~ ~~",
              "         ",
              "  *******",
              "*  ******",
              "** ******",
              "**     **",
              "****** **",
              "*      **",
              "* *******",
              "* *******"]


updateMaze = updateMap snake

move :: [String] -> Int -> Int -> Char -> MazePosition
move l x y mv = let (x1, y1) = case mv of
                                   'i' -> (x, y - 1)
                                   'm' -> (x, y + 1)
                                   'j' -> (x - 1, y)
                                   'k' -> (x + 1, y)
                                   _   -> (-1, -1)
                in
                    if (y1 >= length l) then MazeSolved
                    else if or [x1 < 0, y1 < 0, x1 >= length (l !! y1), ' ' /= l !! y1 !! x1] then MazeOff                             
                    else MazeAt x1 y1
                   

draw :: [String] -> IO ()
draw s = mapM_ putStrLn s >> (mapM_ putStrLn $ take (screenSize - length s) (repeat ""))

mainLoop :: [String] -> Int -> Int -> IO ()
mainLoop l x y = do
                     m <- getChar
                     if m == 'x' then
                         return ()
                     else
                         case move l x y m of
                             MazeOff -> mainLoop l x y
                             MazeAt x1 y1 -> do
                                 draw $ updateMaze l x1 y1
                                 mainLoop l x1 y1
                             MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"

main :: IO ()
main = do
    putStrLn "Hallo! Finde den Weg aus dem Labyrinth!"
    draw $ updateMaze labyrinth 0 0
    mainLoop labyrinth 0 0
