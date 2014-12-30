import qualified Tools as T
import qualified LocalSettings as Settings
import Directory

-- | Calculated position result. Coordinates, forbidden movement or left labyrinth.
data MazePosition = MazeOff         -- ^ Forbidden movement.
                  | MazeAt Position -- ^ New calculated position.
                  | MazeSolved      -- ^ Left labyrinth.

-- | Configuration how to show current position and environment.
data DrawMode = FullSight -- ^ Show whole labyrinth and "ship" with orientation.
              | Sight3d   -- ^ Show only what "ship" can see from its position.
              | SightGfx  -- ^ Show kind of graphics from current position
              deriving (Eq,Enum,Bounded)

-- | One brick of the maze's map
data Brick = Stone | Space | Exit | Ship
           deriving (Eq,Enum,Bounded,Show)

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

-- | Calculate content within maze for position
-- FIXME: Better convert into a maze datatype during read-in
-- and then use functions to retrieve data at position
content :: [String]  -- ^ Map of maze
        -> (Int,Int) -- ^ Coordinates
        -> Brick     -- ^ What is there
content m (x,y) | y < 0 = Stone
                | y >= (length m) = Exit
                | or [x < 0, x >= length (m !! y)] = Stone
                | ' ' /= m !! y !! x = Stone
                | otherwise = Space

-- | Create an abstract view from the current position into the maze.
abstractView :: [String]  -- ^ The map of the maze.
             -> Position  -- ^ The current position to generate the view from.
             -> [[Brick]] -- ^ Up to three rows, the last we stand in the middle.
abstractView m p =
            [
              [
                content m . coord . move Lft . move Fwd . move Fwd $ p,
                content m . coord . move Fwd . move Fwd $ p,
                content m . coord . move Rght . move Fwd . move Fwd $ p
              ],
              [
                content m . coord . move Lft . move Fwd $ p,
                content m . coord . move Fwd $ p,
                content m . coord . move Rght . move Fwd $ p
              ],
              [
                content m . coord . move Lft $ p,
                Ship,
                content m . coord . move Rght $ p
              ]
            ]


-- | Calculate a list of strings for output, reflecting the
-- ship at the current position of the map.
view :: MazeState -- ^ Map, ship direction/position and selected view mode
     -> [String]  -- ^ Output, one line each
view (MazeState m (Position o (x,y)) FullSight) = T.updateMap (ship o) m x y
view (MazeState m p@(Position o (x,y)) Sight3d) =
    let v Stone = '*'
        v Space = ' '
        v Exit = '.'
        v Ship = '^'
        mz = abstractView m p
    in concat [
                [(show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show (length m))],
                map (map v) mz
              ]
view (MazeState m p@(Position o (x,y)) SightGfx) = 
    let mz = abstractView m p
        st = (show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show (length m))
        v Stone = ["/-\\", "| |", "\\-/"]
        v Space = ["   ", "   ", "   "]
        v Exit = ["...", "...", "..."]
        v Ship = ["   ", " ^ ", "   "]
        -- FIXME: ugly, use map
        d l =
            -- let m = map v l
            --    g = map (\x -> x !! 0) m
            --    h = map (\x -> x !! 1) m
            --    l = map (\x -> x !! 2) m
            -- in 
              [
                concat [(v (l !! 0)) !! 0, (v (l !! 1)) !! 0, (v (l !! 2)) !! 0],
                concat [(v (l !! 0)) !! 1, (v (l !! 1)) !! 1, (v (l !! 2)) !! 1],
                concat [(v (l !! 0)) !! 2, (v (l !! 1)) !! 2, (v (l !! 2)) !! 2]
              ]
    in concat [
                [st],
                concat (map d mz)
              ]


-- | Paint String array to screen.
paint :: [String] -> IO ()
paint s = mapM_ putStrLn s >> (mapM_ putStrLn $ take (Settings.screenSize - length s) (repeat ""))

-- | Draw ship. Calculate output using pure function and put result to stdout
draw :: MazeState -> IO ()
draw = paint . view

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

-- | Wait for arbitrary key
getOK :: IO ()
getOK = getChar >> return ()

-- | Query user to select one item from a short list.
selectFrom :: [String]  -- ^ Selection of strings.
           -> IO String -- ^ Selected string.
selectFrom l = do
    mapM_ (\(n,s) -> putStrLn $ concat[show n,":",s]) (zip (take 9 [1 ..]) l)
    putStrLn "Your selection:"
    c <- getChar
    let r = if c `elem` "123456789" then
                read [c] :: Int
            else
                0
    if and [r > 0, r <= length l] then
        return (l !! (r - 1))
      else
        selectFrom l
        

-- | Mainloop containing all IO and state control.
-- FIXME: Split off new state calculation from IO.
mainLoop :: MazeState -> IO ()
mainLoop st = do
    mv <- getChar
    if mv == 'x' then
        return ()
    else if mv == 'h' then
        paint [ "h: This help",
                "",
                " I    -> fwd",
                "J K   -> turn left / right",
                " M    -> bwd",
                "",
                "s: Switch view",
                "x: eXit",
                "",
                "[Hit any key to continue]"] >> getOK >> draw st >> mainLoop st
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
    let path = "."
    files <- T.findFiles path ".map"
    file <- selectFrom files
    mazeString <- readFile (concat [path, "/", file])
    let maze = lines mazeString
    let st = MazeState maze (Position South (0,0)) FullSight
    draw st
    mainLoop st
