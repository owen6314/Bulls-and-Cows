module Guess (
  GameState, 
  initialize, 
  guess,
  refine
) where

import System.Random
import Text.Printf
import Data.List

type GameState = ([Int], Int)
type Result = (Int, Int)

-- data type for choosing new guess
data Candidate = Cand {
    num :: Int, 
    weight :: Double
}

-- sort by weight
instance Eq Candidate where
    Cand _ w1 == Cand _ w2 = w1 == w2
instance Ord Candidate where
    Cand _ w1 <= Cand _ w2 = w1 <= w2

initialize :: (Int -> Bool) -> GameState
initialize p = ([n | n <- [123..9877], p n], 123)

guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (snd s, s)

refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (gs, s) (b, c) = (s', findBestGuess s') 
             where s' = [n | n <- fst s, eval n gs == (b, c)]

-- find next one to guess
findBestGuess :: [Int] -> Int
findBestGuess s = num $ minimum [Cand gs (weightFunc s gs) | gs <- s]

-- Lamouth weight function, can be changed according to different algorithms
weightFunc :: [Int] -> Int -> Double
weightFunc s gs = sum [fromIntegral t * log (fromIntegral t) - 2 * log 2 | t <- rc]
                  where rc = [length t | t <- getIndexGroup s gs] 
                  
-- group index list
getIndexGroup :: [Int] -> Int -> [[Int]]
getIndexGroup s gs = group [getIndex gs mem | mem <- s]

-- get distinct index for each result class
getIndex :: Int -> Int -> Int
getIndex a b = (fst res) * 5 + snd res where res = eval a b

eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

