import qualified Tools as T
import qualified LocalSettings as Settings

-- | Calculated position result. Coordinates, forbidden movement or left labyrinth.
data MazePosition = MazeOff         -- ^ Forbidden movement.
                  | MazeAt Position -- ^ New calculated position.
                  | MazeSolved      -- ^ Left labyrinth.

-- | Configuration how to show current position and environment.
data DrawMode = FullSight -- ^ Show whole labyrinth and "ship" with orientation.
              | Sight3d   -- ^ Show only what "ship" can see from its position.
              deriving (Eq,Enum,Bounded)

-- | A direction for a movement.
data Direction = Lft | Rght | Fwd | Bckw
               deriving (Eq,Enum,Bounded,Show)

-- | Orientation, where ship currently looks into.
data Orientation = North | East | South | West
                 deriving (Eq,Enum,Bounded,Show)

-- | Fully specified coordinates and orientation of "ship"in maze.
data Position = Position { orientation :: Orientation -- ^ In which directiom does the ship look.
                           , coord :: (Int, Int)      -- ^ x,y coordinates, 0,0 being top left
                         }

-- | Current state of game.
data MazeState = MazeState {  mazeMap :: [String]  -- ^ Map of labyrinth
                           ,  position :: Position -- ^ Coordinates including orientation of ship
                           ,  drawMode ::DrawMode  -- ^ Selected view mode.
                           }


-- | Show "ship" for given orientation.
ship :: Orientation -- ^ The direction we look into.
      -> Char       -- ^ A character representing the "ship", looking into the given direction.
ship North = '^'
ship East = '>'
ship South = 'v'
ship West = '<'

-- | Turn ship into given direction, even backwards / forwards. Only changes
-- orientation of position, not coordinates.
turn :: Direction -- ^ Direction to turn 
     -> Position  -- ^ Current position.
     -> Position  -- ^ New position.
turn Lft p = p { orientation = T.rollback . orientation $ p }
turn Rght p = p { orientation = T.roll . orientation $ p }
turn Bckw p = turn Lft . turn Lft $ p
turn Fwd p = id $ p

-- | Move ship one step into given direction. Only change coordinates, not orientation.
move :: Direction -- ^ Direction to move.
     -> Position  -- ^ Current position.
     -> Position  -- ^ New position.
move Fwd p@(Position o (x,y)) = let step East = (x+1,y)
                                    step South = (x, y+1)
                                    step West = (x-1,y)
                                    step North = (x,y-1)
                                in p { coord = step o }
move Lft p = turn Rght . move Fwd . turn Lft $ p
move Rght p = turn Lft . move Fwd . turn Rght $ p
move Bckw p = turn Bckw . move Fwd . turn Bckw $ p

-- | Calculate a list of strings for output, reflecting the
-- ship at the current position of the map.
view :: MazeState -- ^ Map, ship direction/position and selected view mode
     -> [String]  -- ^ Output, one line ech, FIXME: be more abstract for graphical view in opengl
view (MazeState m (Position o (x,y)) FullSight) = T.updateMap (ship o) m x y
view (MazeState m p@(Position o (x,y)) Sight3d) =
    let len = length m
        v (Position _ (x,y)) | y < 0 = '*'
                             | y >= len = '.'
                             | or [x < 0, x >= length (m !! y)] = '*'
                             | ' ' /= m !! y !! x = '*'
                             | otherwise = ' '
    in [(show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show len),
        [v . move Lft . move Fwd . move Fwd $ p,
           v . move Fwd . move Fwd $ p,
           v . move Rght . move Fwd . move Fwd $ p],
        [v . move Lft . move Fwd $ p,
           v . move Fwd $ p,
           v . move Rght . move Fwd $ p],
        [v . move Lft $ p,
           '^',
           v . move Rght $ p] ]

-- | Draw ship. Calculate output using pure function and put to stdout result
draw :: MazeState -> IO ()
draw s = let m = view s
         in mapM_ putStrLn m >> (mapM_ putStrLn $ take (Settings.screenSize - length m) (repeat ""))


-- | Move ship and calculate new position. Return solved, wrong move or new position.
moveShip :: MazeState    -- ^ Game state. FIXME: Only map and position required.
         -> Char         -- ^ Movement. FIXME: Use Direction instead.
         -> MazePosition -- ^ New game state. FIXME: Only position required.
moveShip st mv =
    let p = position st
        (x,y) = coord p
        o = orientation p
        m = mazeMap st
        p1 = case mv of
                 'k' -> turn Rght p
                 'j' -> turn Lft p
                 'i' -> move Fwd p
                 'm' -> move Bckw p
                 _ -> p { coord = (-1,-1) }
        (x1,y1) = coord p1
    in
        if (y1 >= length m) then MazeSolved
        else if or [x1 < 0, y1 < 0, x1 >= length (m !! y1), ' ' /= m !! y1 !! x1] then MazeOff                             
        else MazeAt p1

-- | Mainloop containing all IO and state control.
-- FIXME: Add online help.
-- FIXME: Split off new state calculation from IO.
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
            MazeAt p1 -> do
                let st1 = st { position = p1 }
                draw st1
                mainLoop st1
            MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"

-- | Main function called from Haskell. Initialize maze and start main loop.
main :: IO ()
main = do
    mazeString <- readFile "Labyrinth3d.map"
    let maze = lines mazeString
    let st = MazeState maze (Position South (0,0)) FullSight
    draw st
    mainLoop st
