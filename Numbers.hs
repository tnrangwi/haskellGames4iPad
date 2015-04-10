-- | Well-known game from fun fairs. Get all the numbers from 1 to fifteen
-- in the right order - using the free spot to move around the numbers.
-- FIXME:
-- * Separate printing representation from game state. Currently
-- representation is a mix of the printing representation and coordinates,
-- which is nonsense because the printing representation already contains
-- the coordinates. Better use a list of integers as representation and
-- only convert to print representation on drawing.
-- * Generalize draw funktion to be able to print a help as well.
import qualified Tools as T
import qualified LocalSettings as Settings
import qualified Text.Printf as Pf
import qualified Data.List as DL

data MovePosition = MoveOff
                  | MoveSwap Int Int
                  | MoveSolved

-- | Draw the current status, adding enough line feeds to emulate a clear screen.
draw :: [[String]] -- ^ The current lines of the number map
     -> IO ()      -- ^ Printing adds IO
draw m = mapM_ (putStrLn . concat) m >> (mapM_ putStrLn $ take (Settings.screenSize - length m) (repeat ""))

-- | Given the map of numbers, calculate the new position - if move is valid
move :: [[String]] -- ^ The current map of numbers as lines
     -> Int        -- ^ x coordinate of empty spot
     -> Int        -- ^ y coordinate of empty spot
     -> Char       -- ^ the character read from stdin giving the required movement
     -> Maybe (Int,Int) -- ^ the new position of the empty spot, None on invalid move
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

-- | Whith a given valid movement calculate the new 2-dimensional
-- representation of the map. This is printable. Currently one of
-- the coordinates contains the empty spot, the other one contains
-- the number and both coordinates are just exchanged in content.
roll :: [[String]] -- ^ Current 2-dimensional representation.
     -> (Int,Int)  -- ^ 1st 2-dimensional coordinate to exchange.
     -> (Int,Int)  -- ^ 2nd 2-dimensional coordinate to exchange.
     -> [[String]] -- ^ The resukting new map.
roll m (x,y) (x1,y1) = let s = m !! y !! x
                           s1 = m !! y1 !! x1
                           u1 = T.updateAt m y (T.updateAt (m !! y) x s1)
                       in
                           T.updateAt u1 y1 (T.updateAt (u1 !! y1) x1 s)

-- | Check whether representation of number field is already solved.
finalForm :: [[String]] -- ^ String representation of the number's map
          -> Bool       -- ^ Solved state or not
finalForm m = let fl = concat m
                  l = length fl
                  o (x:y:ys) = and [x<y,o (y:ys)]
                  o _ = True
              in
                  and [" .. " == fl !! (l-1),o (take (l - 1) fl)]

-- | From the given range of integers calculate the 2-dimensional coordinates
-- of the empty spot.
getStart :: [Int] -> (Int,Int)
getStart m = let i = DL.elemIndex 1 m
             in
                 case i of
                     Just p -> (p `rem` 4, p `quot` 4)
                     Nothing -> error "Can't find start"

-- | Given a list of unique numbers generate a 2-dimensional
-- map, represented as lines, numbers converted into the corresponding
-- characters, 0 representing the empty spot as two dots.
-- Limitation: Always splits at 4 numbers given.
getNumberMap :: [Int]      -- ^ the list of unique numbers to form a map of
             -> [[String]] -- ^ the resukting 2-dimensional map
getNumberMap l = let sp s = " " ++ s ++ " "
                     m 0 = sp ".."
                     m i = sp $ Pf.printf "%02d" i
                     chunk [] = []
                     chunk l = let
                                   (b,e) = splitAt 4 l
                               in
                                    [b] ++ (chunk e)
                 in chunk $ map m l

-- | Main loop. Everything drawn, now accept directions from
-- stdin. Read character and re-loop or do action and reloop
-- or terminate when game is solved after action.
mainLoop :: [[String]] -- ^ Map with current representation
         -> Int        -- ^ x coordinate of empty spot on map
         -> Int        -- ^ y coordinate of empty spot on map
         -> IO ()      -- ^ when returning, then game is done
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

-- | Main function. Generate the random numbers for construction of the game,
-- prepare map, draw first map and call mainloop with map and 2-dimensional
-- coordinates of the empty spot.
main :: IO ()
main = do
    box <- T.getRandomSequence 16
    let gamesMap = getNumberMap $ map pred box
    draw gamesMap
    let (x,y) = getStart box
    mainLoop gamesMap x y
