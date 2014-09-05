module Tools where

import System.Time
import System.Random

-- |Update a sequence at the given position with the
-- given element. Return original if index is out of
-- bounds.
updateAt :: [a] ->  Int -> a -> [a]
updateAt l i c = if or [i < 0, i >= length l] then
                     l
                 else
                     take i l ++ [c] ++ drop (i + 1) l

-- |Update a 2-dimensional map given as kist of strings
-- with the given character at the given x / y 
-- coordinates.
updateMap :: Char -> [String] -> Int -> Int -> [String]
updateMap c m x y = if or [y < 0, x < 0, y >= length m, x >= length (m !! y)] then
                       m
                   else
                       updateAt m y (updateAt (m !! y) x c)

-- |Get a random sequence of given length, using the
-- given std random number generator. All numbers from
-- 1 untik the given integer occur exactly once.
-- This may use
-- and arbitrary number of random numbers from the
-- generator until the list is complete.
randomUniqueSequence :: Int -> StdGen -> [Int]
randomUniqueSequence x g = gen [] g
        where
            gen l g = if length l == x then
                          l
                      else
                           let (q, g1) = randomR (1, x) g
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
getRandomSequence :: Int -> IO [Int]
getRandomSequence i =
  getClockTime >>= return . randomUniqueSequence i . mkStdGen . ctSec . toUTCTime

