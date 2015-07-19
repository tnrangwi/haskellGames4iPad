import qualified Tools as T
import qualified LocalSettings as Settings
import qualified Control.Monad as CM

-- | Calculated position result. Coordinates, forbidden movement or left labyrinth.
data MazePosition = MazeOff (Maybe Collectable) -- ^ Forbidden movement with optional missing item.
                  | MazeAt Position -- ^ New calculated position.
                  | MazeSolved      -- ^ Left labyrinth.
                  | MazeItem Position Collectable

-- | Configuration how to show current position and environment.
data DrawMode = FullSight -- ^ Show whole labyrinth and "ship" with orientation.
              | Sight3d   -- ^ Show only what "ship" can see from its position.
              | SightGfx  -- ^ Show kind of graphics from current position
              deriving (Eq,Enum,Bounded)

-- | One brick of the maze's map
data Brick = Stone | Space | Exit | Ship | RedDoor | YellowDoor | GreenDoor | Item Collectable
           deriving (Eq,Show)

-- | Anything that can be picked up in the labyrinth
data Collectable = Maps | RedKey | YellowKey | GreenKey
                 deriving (Eq,Show)

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
data MazeState = MazeState {  mazeMap  :: [String]      -- ^ Map of labyrinth
                           ,  position :: Position      -- ^ Coordinates including orientation of ship
                           ,  drawMode :: DrawMode      -- ^ Selected view mode.
                           ,  items    :: [Collectable] -- ^ Items collected up to now
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
-- and then use functions to retrieve data at position. Currently the maze is converted at
-- any move and then back. Better do that once.
content :: [String]  -- ^ Map of maze
        -> (Int,Int) -- ^ Coordinates
        -> Brick     -- ^ What is there
content m (x,y) | y < 0 = Stone
                | y >= (length m) = Exit
                | or [x < 0, x >= length (m !! y)] = Stone
                | 'M' == m !! y !! x = Item Maps
                | 'r' == m !! y !! x = Item RedKey
                | 'g' == m !! y !! x = Item GreenKey
                | 'y' == m !! y !! x = Item YellowKey
                | 'R' == m !! y !! x = RedDoor
                | 'G' == m !! y !! x = GreenDoor
                | 'Y' == m !! y !! x = YellowDoor
                | ' ' /= m !! y !! x = Stone
                | otherwise = Space

-- | Select a view mode depending on device
selectView :: DrawMode -- ^ Simple or detailed view mode
selectView = if Settings.screenDetail then SightGfx else Sight3d



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
view :: [String]  -- ^ Currently used map
     -> Position  -- ^ Positionwithin the map
     -> DrawMode  -- ^ Selected view mode
     -> [String]  -- ^ Output, one line each
view m (Position o (x,y)) FullSight = T.updateMap (ship o) m x y
view m p@(Position o (x,y)) Sight3d =
    let v Stone = '*'
        v Space = ' '
        v Exit = '.'
        v Ship = '^'
        v RedDoor = 'R'
        v YellowDoor = 'Y'
        v GreenDoor = 'G'
        v (Item Maps) = 'M'
        v (Item RedKey) = 'r'
        v (Item YellowKey) = 'y'
        v (Item GreenKey) = 'g'
        mz = abstractView m p
    in concat [
                [(show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show (length m))],
                map (map v) mz
              ]
view m p@(Position o (x,y)) SightGfx = 
    let mz = abstractView m p
        st = (show x) ++ "/" ++ (show y) ++ "/" ++ (show o) ++ "/" ++ (show (length m))
        v Stone = ["/-\\", "| |", "\\-/"]
        v Space = ["   ", "   ", "   "]
        v Exit = ["...", "...", "..."]
        v Ship = ["   ", " ^ ", "   "]
        v RedDoor = ["\\ /"," L ","/ \\"]
        v YellowDoor = ["\\ /"," F ","/ \\"]
        v GreenDoor = ["\\ /"," E ","/ \\"]
        v (Item Maps) = [" _ ","|M|","   "]
        v (Item RedKey) = ["<| "," | "," O "]
        v (Item YellowKey) = [" | ","<| "," O "]
        v (Item GreenKey) = ["<| ","<| "," O "]
        matrix = map (T.mergeRectangle . map v) mz
    in concat [
                [st],
                concat matrix
              ]

-- | Draw ship. Calculate output using pure function and put result to stdout
draw :: MazeState -> IO ()
draw st = let m = mazeMap st
              p = position st
              d = drawMode st
          in T.paint $ view m p d

movement :: Char            -- ^ Movement character aa chosen by the console
         -> Maybe Direction -- ^ Converted: direction to move into
movement 'i' = Just Fwd
movement 'm' = Just  Bckw
movement 'j' = Just Lft
movement 'k' = Just Rght
movement _  = Nothing

-- | Move ship and calculate new position. Return solved, wrong move or new position.
moveShip :: MazeState       -- ^ Game state.
         -> Maybe Direction -- ^ Direction to move
         -> MazePosition    -- ^ New game state.
moveShip _ Nothing = MazeOff Nothing
moveShip st (Just mv) =
    let p = position st
        (x,y) = coord p
        m = mazeMap st
        its = items st
        p1 = case mv of
                 Rght -> turn Rght p
                 Lft -> turn Lft p
                 Fwd -> move Fwd p
                 Bckw -> move Bckw p
                 _ -> p { coord = (-1,-1) }
        -- Check if item is there, then proceed, else fail
        miss i = if i `elem` its then MazeAt p1 else MazeOff (Just i)
        (x1,y1) = coord p1
    in
        if (y1 >= length m) then MazeSolved
        else if or [x1 < 0, y1 < 0, x1 >= length (m !! y1)] then MazeOff Nothing
        else if 'M' == m !! y1 !! x1 then MazeItem p1 Maps
        else if 'r' == m !! y1 !! x1 then MazeItem p1 RedKey
        else if 'y' == m !! y1 !! x1 then MazeItem p1 YellowKey
        else if 'g' == m !! y1 !! x1 then MazeItem p1 GreenKey
        else if 'R' == m !! y1 !! x1 then miss RedKey
        else if 'Y' == m !! y1 !! x1 then miss YellowKey
        else if 'G' == m !! y1 !! x1 then miss GreenKey
        else if ' ' /= m !! y1 !! x1 then MazeOff Nothing
        else MazeAt p1

-- | Wait for arbitrary key
getOK :: IO ()
getOK = getChar >> return ()

-- | Query user to select one item from a short list.
-- The query lasts until one element is selected, so
-- it has to fail if the list is empty.
selectFrom :: [String]  -- ^ Selection of strings.
           -> IO String -- ^ Selected string.
selectFrom [] = error "Empty list - program aborted to break endless loop"
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
        T.paint [ "h: This help",
                "",
                " I    -> fwd",
                "J K   -> turn left / right",
                " M    -> bwd",
                "",
                "s: Switch to map - if there",
                "a: Show what you carry.",
                "H: game help",
                "x: eXit",
                "",
                "[Hit any key to continue]"] >> getOK >> draw st >> mainLoop st
    else if mv == 'H' then
        T.paint ["Search the exit at the South.",
               "Collect map to make it available",
               "via the s key. Collect keys to",
               "open the corresponding doors.",
               "Just walk on items to collect them.",
               "Map shows red / yellow / green",
               "keys with small letters and",
               "corresponding doors with",
               "capital letters"] >> getOK >> draw st >> mainLoop st
    else if mv == 'a' then
        T.paint ("You carry:" : (map show (items st))) >> getOK >> draw st >> mainLoop st
    else if mv == 's' then
        if Maps `elem` (items st) then
            draw st { drawMode = FullSight } >> getOK >> draw st >> mainLoop st
        else
            mainLoop st
    else
        case (moveShip st (movement mv)) of
            MazeOff Nothing -> mainLoop st
            MazeOff (Just miss) -> T.paint ["Missing:" ++ (show miss)] >> getOK >> draw st >> mainLoop st
            MazeAt p1 -> do
                let st1 = st { position = p1 }
                draw st1
                mainLoop st1
            MazeSolved -> putStrLn "Herzlichen Glueckwunsch. Du hast es geschafft!"
            MazeItem p1 i -> do
                let m = mazeMap st
                let (x,y) = coord p1
                let m1 = T.updateMap ' ' m x y
                let items1 = i:(items st)
                let st1 = st { position = p1, mazeMap = m1, items = items1 }
                T.paint ["You collected:" ++ (show i)]
                getOK
                draw st1
                mainLoop st1

-- | Main function called from Haskell. Initialize maze and start main loop.
main :: IO ()
main = do
    let path = "Labyrinth3d"
    files <- T.findFiles path ".map"
    CM.when (null files) (fail "Setup error, no maps found!")
    file <- selectFrom files
    mazeString <- readFile (concat [path, "/", file])
    let maze = lines mazeString
    let st = MazeState maze (Position South (0,0)) (selectView) []
    draw st
    mainLoop st
