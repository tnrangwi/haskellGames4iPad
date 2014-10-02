-- | This was the very first game. Simple labyrinth in full view
-- to navigate a snake to the bottom.
import qualified Tools as T
import qualified LocalSettings as Settings

data MazePosition = MazeOff
                  | MazeAt Int Int
                  | MazeSolved

-- | Character to use for the snake. A constant.
snake :: Char
snake = '~'

-- | A representation of the labyrinth
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

-- | Update maze by inserting the snake (i.e., an arbitrary character) at the given position.
updateMaze :: [String] -- ^ input map
           -> Int      -- ^ x ccordinate for insert, counting left to right
           -> Int      -- ^ y coordinate for insert, counting top to down
           -> [String] -- ^ resulting map, together with inserted snake.
updateMaze = T.updateMap snake

-- | Calculate a new position in the maze, given mp, position and movement character.
move :: [String] -- ^ The map of the maze.
     -> Int      -- ^ x coordinate within the maze, counting left to right
     -> Int      -- ^ y coordinate within the maze, counting up to down.
     -> Char     -- ^ Movemebt character, exactly what was captured by getChar in
                 -- main loop. i/m -> up/down, j/k -> left/right.
     -> MazePosition -- Resulting new position after movement. Off, exited or bew position.
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
                   
-- | Draw maze or whatever to stdout, filling up with as much lines as the screen provides 
-- for a "full" redraw of the while screen.
draw :: [String] -> IO ()
draw s = mapM_ putStrLn s >> (mapM_ putStrLn $ take (Settings.screenSize - length s) (repeat ""))

-- | mainloop, startuing with a map and a position within. Query movement interactively
-- from stdin, calculate new position and call itself. If new position is valid, redraw
-- screen. If new pisition is a vakid movement to leave tge maze - then exit.
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

-- | Show initial maze and call mainloop.
main :: IO ()
main = do
    putStrLn "Hallo! Finde den Weg aus dem Labyrinth!"
    draw $ updateMaze labyrinth 0 0
    mainLoop labyrinth 0 0
