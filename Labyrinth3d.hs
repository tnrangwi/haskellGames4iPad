import qualified Tools as T
import qualified LocalSettings as Settings

data MazePosition = MazeOff
                  | MazeAt Int Int Char
                  | MazeSolved

data DrawMode = FullSight
              | Sight3d
              deriving (Eq,Enum,Bounded)

data MazeState = MazeState { mazeMap :: [String],
                             coordX :: Int,
                             coordY :: Int,
                             coordO :: Char,
                             drawMode ::DrawMode }

data Direction = Lft | Rght | Fwd | Bckw
               deriving (Eq,Enum,Bounded,Show)

data Orientation = North | East | South | West
                 deriving (Eq,Enum,Bounded,Show)

ship :: Orientation -> Char
ship North = '^'
ship East = '>'
ship South = 'v'
ship West = '<'

-- | advance one step into given direction
advance :: Direction -> Orientation -> (Int,Int) -> (Int, Int)
advance Fwd East (x,y) = (x+1,y)
advance Fwd South (x,y) = (x,y+1)
advance Fwd West (x,y) = (x-1,y)
advance Fwd North (x,y) = (x,y-1)
advance Lft o p = advance Fwd (T.rollback o) p
advance Rght o p = advance Fwd (T.roll o) p
advance Bckw o p = advance Fwd (T.roll . T.roll $ o) p

draw :: MazeState -> IO ()
draw s = let c = coordO s
             m = mazeMap s
             x = coordX s
             y = coordY s
             left '>' (x,y) = (x,y-1)
             left 'v' (x,y) = (x+1,y)
             left '<' (x,y) = (x,y+1)
             left '^' (x,y) = (x-1,y)
             right '>' (x,y) = (x,y+1)
             right 'v' (x,y) = (x-1,y)
             right '<' (x,y) = (x,y-1)
             right '^' (x,y) = (x+1,y)
             fwd '>' (x,y) = (x+1,y)
             fwd 'v' (x,y) = (x,y+1)
             fwd '<' (x,y) = (x-1,y)
             fwd '^' (x,y) = (x,y-1)
             len = length m
             l = left c
             r = right c
             f = fwd c
             v (x,y) | y < 0 = '*'
                     | y >= len = '.'
                     | or [x < 0, x >= length (m !! y)] = '*'
                     | ' ' /= m !! y !! x = '*'
                     | otherwise = ' '
             view = [[v . l . f . f $ (x,y), v . f . f $ (x,y), v . r . f . f $ (x,y)],
                     [v . l . f $ (x,y), v . f $ (x,y), v . r . f $ (x,y)],
                     [v . l $ (x,y), v (x,y), v . r $ (x,y)] ]
         in
             if drawMode s == FullSight then
                 mapM_ putStrLn (T.updateMap c m x y) >> 
                     (mapM_ putStrLn $ take (Settings.screenSize - length m) (repeat ""))
             else
                 do
                     putStrLn $ (show x) ++ "/" ++ (show y) ++ "/" ++ [c] ++ "/" ++ (show $ length m)
                     mapM_ putStrLn view
                     mapM_ putStrLn $ take (Settings.screenSize - 1 - (length view)) (repeat "")

moveShip :: MazeState -> Char -> MazePosition
moveShip st mv =
    let x = coordX st
        y = coordY st
        c = coordO st
        m = mazeMap st
        (x1,y1,c1) = case (c,mv) of
                         ('v','k') -> (x,y,'<') -- FIXME: implement by roll
                         ('<','k') -> (x,y,'^')
                         ('^','k') -> (x,y,'>')
                         ('>','k') -> (x,y,'v')
                         
                         ('v','j') -> (x,y,'>') --FIXME: implement by rollback
                         ('<','j') -> (x,y,'v')
                         ('^','j') -> (x,y,'<')
                         ('>','j') -> (x,y,'^')
                         
                         ('v','i') -> (x,y+1,c) --FIXME: implement by advance
                         ('<','i') -> (x-1,y,c)
                         ('^','i') -> (x,y-1,c)
                         ('>','i') -> (x+1,y,c)
                         
                         ('v','m') -> (x,y-1,c) --FIXME: implement by advance
                         ('<','m') -> (x+1,y,c)
                         ('^','m') -> (x,y+1,c)
                         ('>','m') -> (x-1,y,c)
                         _ -> (-1,-1,c)
    in
        if (y1 >= length m) then MazeSolved
        else if or [x1 < 0, y1 < 0, x1 >= length (m !! y1), ' ' /= m !! y1 !! x1] then MazeOff                             
        else MazeAt x1 y1 c1

mainLoop :: MazeState -> IO ()
mainLoop st = do
    mv <- getChar
    if mv == 'x' then
        return ()
    else if mv == 's' then
        let st1 = st { drawMode = T.roll $ drawMode st } in
        draw st1 >> mainLoop st1
    else
        case moveShip st mv of
            MazeOff -> mainLoop st
            MazeAt x1 y1 c1 -> do
                let st1 = st { coordX = x1, coordY = y1, coordO = c1 }
                draw st1 -- $ updateMap c1 m x1 y1
                mainLoop st1
            MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"


main :: IO ()
main = do
    mazeString <- readFile "Labyrinth3d.map"
    let maze = lines mazeString
    let st = MazeState maze 0 0 'v' FullSight
    draw st -- $ updateMap maze 0 0 'v'
    mainLoop st
