module Guess (
  GameState, 
  initialize, 
  guess,
  refine
) where

import System.Random
import Text.Printf

type GameState = ([Int], Int)
type Result = (Int, Int)
data Candidate = Cand {
    num :: Int, 
    weight :: Double
}

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

findBestGuess :: [Int] -> Int
findBestGuess s = num $ maximum [Cand gs (weightFunc (genRc s gs (replicate 14 0)) 0) | gs <- s]

-- generate response classes
genRc :: [Int] -> Int -> [Int] -> [Int]
genRc s gs r
  | s == [] = r
  | otherwise = genRc (tail s) gs ((take index r) ++ [(r !! index)] ++ (drop (index + 1) r))
  where index = getIndex (head s) gs

getIndex :: Int -> Int -> Int
getIndex a b
  | fst res <= 1 = (fst res) * 4 + snd res  
  | fst res == 2 = (fst res) * 4 + snd res - 1
  | fst res == 3 = 12
  | fst res == 4 = 13
  where res = eval a b

weightFunc :: [Int] -> Double -> Double
weightFunc [] n = n
weightFunc ns n 
  | head ns == 0 = weightFunc (tail ns) n
  | otherwise = weightFunc (tail ns) (n + t * (log t) - 2 * log 2) where t = fromIntegral $ head ns

eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

