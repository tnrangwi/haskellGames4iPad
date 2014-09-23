import qualified Tools as T
import qualified LocalSettings as Settings

data MazePosition = MazeOff
                  | MazeAt Int Int Orientation
                  | MazeSolved

data DrawMode = FullSight
              | Sight3d
              deriving (Eq,Enum,Bounded)

data Direction = Lft | Rght | Fwd | Bckw
               deriving (Eq,Enum,Bounded,Show)

data Orientation = North | East | South | West
                 deriving (Eq,Enum,Bounded,Show)

data MazeState = MazeState { mazeMap :: [String],
                             coordX :: Int,
                             coordY :: Int,
                             coordO :: Orientation,
                             drawMode ::DrawMode }


-- | show ship char for selected orientation
ship :: Orientation -> Char
ship North = '^'
ship East = '>'
ship South = 'v'
ship West = '<'

-- | turn ship into given direction
turn :: Direction -> (Orientation,(Int,Int)) -> (Orientation,(Int,Int))
turn Lft (o,s) = (T.rollback o,s)
turn Rght (o,s) = (T.roll o, s)
turn Bckw p = turn Lft . turn Lft $ p
turn Fwd p = id $ p

-- | move ship one step into given direction
move :: Direction -> (Orientation, (Int,Int)) -> (Orientation, (Int, Int))
move Fwd (o,(x,y)) = let step East = (x+1,y)
                         step South = (x, y+1)
                         step West = (x-1,y)
                         step North = (x,y-1)
                     in (o,step o)
move Lft p = turn Rght . move Fwd . turn Lft $ p
move Rght p = turn Lft . move Fwd . turn Rght $ p
move Bckw p = turn Bckw . move Fwd . turn Bckw $ p

-- | Draw ship. FIXME: Split up into IO part and picture calculation.
draw :: MazeState -> IO ()
draw s = let o = coordO s
             m = mazeMap s
             x = coordX s
             y = coordY s
             p = (o,(x,y))

             len = length m
             v (_,(x,y)) | y < 0 = '*'
                         | y >= len = '.'
                         | or [x < 0, x >= length (m !! y)] = '*'
                         | ' ' /= m !! y !! x = '*'
                         | otherwise = ' '
             view = [[v . move Lft . move Fwd . move Fwd $ p,
                            v . move Fwd . move Fwd $ p,
                            v . move Rght . move Fwd . move Fwd $ p],
                       [v . move Lft . move Fwd $ p,
                            v . move Fwd $ p,
                            v . move Rght . move Fwd $ p],
                       [v . move Lft $ p,
                            '^',
                            v . move Rght $ p] ]
         in
             if drawMode s == FullSight then
                 mapM_ putStrLn (T.updateMap (ship o) m x y) >> 
                     (mapM_ putStrLn $ take (Settings.screenSize - length m) (repeat ""))
             else
                 do
                     putStrLn $ (show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show $ length m)
                     mapM_ putStrLn view
                     mapM_ putStrLn $ take (Settings.screenSize - 1 - (length view)) (repeat "")

-- | Move ship and calculate new position. Return solved, wrong move or new position.
moveShip :: MazeState -> Char -> MazePosition
moveShip st mv =
    let x = coordX st
        y = coordY st
        o = coordO st
        p = (o,(x,y))
        m = mazeMap st
        new@(o1,(x1,y1)) = case mv of
                  'k' -> turn Rght p
                  'j' -> turn Lft p
                  'i' -> move Fwd p
                  'm' -> move Bckw p
                  _ -> (o,(-1,-1))

        
    in
        if (y1 >= length m) then MazeSolved
        else if or [x1 < 0, y1 < 0, x1 >= length (m !! y1), ' ' /= m !! y1 !! x1] then MazeOff                             
        else MazeAt x1 y1 o1

-- | Mainloop containing all IO and state control.
-- FIXME: Add online help.
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
            MazeAt x1 y1 o1 -> do
                let st1 = st { coordX = x1, coordY = y1, coordO = o1 }
                draw st1
                mainLoop st1
            MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"

-- | Main function called from Haskell. Initialize maze and start main loop.
main :: IO ()
main = do
    mazeString <- readFile "Labyrinth3d.map"
    let maze = lines mazeString
    let st = MazeState maze 0 0 South FullSight
    draw st
    mainLoop st
