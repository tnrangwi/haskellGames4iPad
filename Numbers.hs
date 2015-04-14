-- | Well-known game from fun fairs. Get all the numbers from 1 to fifteen
-- in the right order - using the free spot to move around the numbers.
import qualified Tools as T
import qualified LocalSettings as Settings
import qualified Text.Printf as Pf
import qualified Data.List as DL

data MovePosition = MoveOff
                  | MoveSwap Int Int
                  | MoveSolved

data Direction = Up
               | Down
               | Lft
               | Rght
               deriving (Show, Eq, Enum, Bounded)

data HandleMove = HandleExit
                | HandleMove Int Int
                | HandleInvalid
                deriving (Show, Eq)

-- | Find position of enpty spot. Return error because it must be there.
index :: [Int] -- ^ The Integer array with all numbers
      -> Int   -- ^ The index where the empty spot is.
index m = case DL.elemIndex 0 m of
            Just i -> i
            Nothing -> error "Internal error, representation requires 0-marker"

-- | Calculate the new position when moving the empty spot. Check for boundaries.
-- a left or right movement may well keep the cursor in the 1-dimensional range,
-- but a check for 2-dimensional conversion makes sure to catch an invalid movement.
calculate :: [Int]           -- ^ The array od numbers.
          -> Direction       -- ^ The direction to move the cursor / empty spot
          -> Maybe (Int,Int) -- ^ The new and old position of the cursor or Nothing on invalid move.
calculate m d = let c = index m -- where is cursor
                    l = getLength $ length m
                    n = case d of
                            Up -> c - l
                            Down -> c + l
                            Lft -> c - 1
                            Rght -> c + 1
                    in
                      if or[d == Lft,d == Rght] then
                        if (n `div` l) == (c `div` l) then Just (n,c) else Nothing
                      else
                        if and [n >= 0,n < length m] then Just (n,c) else Nothing

-- | Calculate the length of every line in Int.
-- Input must be a square, otherwise error is returned.
-- Further processing does not make sense when number
-- is no square.
getLength :: Int -- ^ The full length of the array.
          -> Int -- ^ The number of items by line when split
                 -- into equal lines and columns.
getLength i = let r = floor . sqrt . fromIntegral $ i
              in if i == r * r then r else error "No square, further processing would fail."

-- | Draw the current status, adding enough line feeds to emulate a clear screen.
draw :: [[String]] -- ^ The current lines of the number map
     -> IO ()      -- ^ Printing adds IO
draw m = mapM_ (putStrLn . concat) m >> (mapM_ putStrLn $ take (Settings.screenSize - length m) (repeat ""))

finalForm :: [Int]
          -> Bool
finalForm m = let o (x:y:ys) = and [x<y,o (y:ys)]
                  o _ = True
                  l = length m
              in and [0 == m !! (l-1), o (take (l-1) m)]

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

loop :: [Int]
     -> IO ()
loop m = do
    mv <- getChar
    case hCmd mv of
      HandleExit -> return ()
      HandleMove x y -> do
        let vx = m !! x
        let vy = m !! y
        let m1 = T.updateAt (T.updateAt m x vy) y vx
        draw $ getNumberMap m1
        if finalForm m1 then
          putStrLn "Herzlichen Glueckwunsch, Du hast es geschafft!"
        else
          loop m1
      HandleInvalid -> loop m
  where
    hCmd c
      | c == 'x' = HandleExit
      | c `elem` "ijkm" = hMov c
    hMov 'i' = testMov Down
    hMov 'm' = testMov Up
    hMov 'j' = testMov Rght
    hMov 'k' = testMov Lft
    testMov d = case calculate m d of
                  Just (x,y) -> HandleMove x y
                  Nothing -> HandleInvalid


-- | Main function. Generate the random numbers for construction of the game,
-- prepare map, draw first map and call mainloop with map and 2-dimensional
-- coordinates of the empty spot.
main :: IO ()
main = do
    box <- T.getRandomSequence 16
    let gamesMap = map pred box
    draw $ getNumberMap gamesMap
    loop gamesMap
