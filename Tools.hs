module Tools

(
  updateAt,
  updateMap,
  getRandomSequence,
  roll,
  rollback
)

where

import qualified System.Time as T
import qualified System.Random as R

-- |Update a sequence at the given position with the
-- given element. Return original if index is out of
-- bounds.
updateAt :: [a] ->  Int -> a -> [a]
updateAt l i c = if or [i < 0, i >= length l] then
                     l
                 else
                     take i l ++ [c] ++ drop (i + 1) l

-- |Update a 2-dimensional map given as list of strings
-- with the given character at the given x / y 
-- coordinates.
updateMap :: Char -> [String] -> Int -> Int -> [String]
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
-- usw pure code to generate a large number of randoms.
getRandomSequence :: Int -> IO [Int]
getRandomSequence i =
  T.getClockTime >>= return . randomUniqueSequence i . R.mkStdGen . T.ctSec . T.toUTCTime
-- |Roll a finite enumeration to the next value -- switching to the first when the last
-- value of the enumeration is reached.
roll :: (Enum a, Bounded a, Eq a) => a -> a
roll s | s == maxBound = toEnum 0
       | otherwise = succ s

rollback :: (Enum a, Bounded a, Eq a) => a -> a
rollback s | s == minBound = toEnum maxBound
           | otherwise = pred s



