-- | Collection of tool functions needed by the game implementations.
-- All conten in one Tool module now, mixing IO functions for getting random
-- numbers with pure functions for list manipulations.
module Tools

(
  updateAt,
  updateMap,
  getRandomSequence,
  roll,
  rollback,
  endsWith,
  findFiles
)

where

import qualified System.Time as T
import qualified System.Random as R
import qualified System.Directory as D

-- |Update a sequence at the given position with the
-- given element. Return original if index is out of
-- bounds.
updateAt :: [a]  -- ^ Lust to update.
         ->  Int -- ^ Update at position.
         -> a    -- ^ Update with thatcharacter.
         -> [a]  -- ^ Updated list.
updateAt l i c = if or [i < 0, i >= length l] then
                     l
                 else
                     take i l ++ [c] ++ drop (i + 1) l

-- |Update a 2-dimensional map given as list of strings
-- with the given character at the given x / y 
-- coordinates.
updateMap :: Char     -- ^ Character to put into 2-dimensional map
          -> [String] -- ^ line by line the lines of the map, left-justified, but
                      -- possibly of different length. Off the array meens off the map.
          -> Int      -- ^ x coordinate of the position for the char to insert.
          -> Int      -- ^ y coordinate of the position for the char to insert.
          -> [String] -- ^ resulting map with additional character.
updateMap c m x y = if or [y < 0, x < 0, y >= length m, x >= length (m !! y)] then
                       m
                   else
                       updateAt m y (updateAt (m !! y) x c)

-- |Get a random sequence of given length, using the
-- given std random number generator. All numbers from
-- 1 until the given integer occur exactly once.
-- This may use an arbitrary number of random numbers from the
-- generator until the list is complete.
randomUniqueSequence :: Int -> R.StdGen -> [Int]
randomUniqueSequence x g = gen [] g
        where
            gen l g = if length l == x then
                          l
                      else
                           let (q, g1) = R.randomR (1, x) g
                           in
                               if q `elem` l then
                                   gen l g1
                               else
                                   gen (q:l) g1

-- |Get a random sequence starting from 1 up to
-- the given Int.
-- Current clocktime seconds are used to initialize
-- the random number generator. This is suitable enough
-- when a game just needs once such a series at startup.
-- It is not, when sequences are needes quite often. In
-- that case better fetch the random number generator and
-- use pure code to generate a large number of randoms.
getRandomSequence :: Int -> IO [Int]
getRandomSequence i =
  T.getClockTime >>= return . randomUniqueSequence i . R.mkStdGen . T.ctSec . T.toUTCTime

-- |Roll a finite enumeration to the next value -- switching to the first when the last
-- value of the enumeration is reached.
roll :: (Enum a, Bounded a, Eq a) => a -> a
roll s | s == maxBound = minBound
       | otherwise = succ s

-- | Same as roll, but roll backwards, i.e. choose the next "smaller" element
-- from the given enumeration.
rollback :: (Enum a, Bounded a, Eq a) => a -> a
rollback s | s == minBound = maxBound
           | otherwise = pred s

-- | Check whether a list ends with a given sequence
endsWith :: Eq a
         => [a] -- ^ suffix to compare
         -> [a] -- ^ target to check
         -> Bool
endsWith comp target = (drop (max 0 ((length target) - (length comp))) target) == comp

-- | Find files with given extension in directory
findFiles :: FilePath -> String -> IO [String]
findFiles path suffix = do
    l <- D.getDirectoryContents path
    return [ x | x <- l, endsWith suffix x ]
