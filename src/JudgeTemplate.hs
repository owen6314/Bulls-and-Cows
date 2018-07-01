module JudgeTemplate where

import           Control.DeepSeq          (NFData, deepseq)
import           Control.Exception        (SomeException, try)
import           Control.Monad.State.Lazy
import           Control.Monad.Writer
import           Data.List
import           Guess                    (GameState, guess, initialize, refine)
import           System.CPUTime           (getCPUTime)
import           System.Random
import           System.Timeout           (timeout)
import           Text.Printf              (printf)

-- Basic utilities --

ite :: Bool -> a -> a -> a
ite b x y = if b then x else y

-- pretty printing of 4 digits
show04 :: Int -> String
show04 = printf "%04d"

-- blast the int to list of 4 int. E.g. blast 123 = [0,1,2,3]
blast :: Int -> [Int]
blast n = reverse $ take 4 $ map (`mod` 10) $ iterate (`div` 10) n

-- interpreation of a predicate
interprete :: (Int -> Bool) -> [Int]
interprete p = error "maybe implement it"

-- take a few numbers from a list
sample :: Int -> [a] -> [a]
sample n xs = go 1 xs where
  go _ [] = []
  go m (x:xs)
    | m == n = x: go 1 xs
    | otherwise = go (m+1) xs

-- Definitions --

-- 30 secs.
maxTimeOut :: Int
maxTimeOut = 30 * 10^6

-- 10 attempts
maxAttempts :: Int
maxAttempts = 10

-- valid number: 4 digit nunbers with no duplicated digit
valid :: Int -> Bool
valid n = 0000 <= n && n <= 9999 && noDup n where
  noDup x = length (Data.List.nub $ blast n) == 4

-- Various predicates
p1 <&> p2 = \x -> p1 x && p2 x
-- all valid number
predAll = valid
-- only odd number
predOdd = valid <&> (all odd . blast)
-- only even number
predEven = valid <&> (all even . blast)
-- small than 5
predSmall = valid <&> (all (< 5) . blast)
-- larger than 4
predLarge = valid <&> (all (>= 5) . blast)
-- sum less than 20
predSmallSum = valid <&> (\n -> digitSum n < 20) where
  digitSum m
    | m == 0 = 0
    | otherwise = (m `mod` 10) + digitSum (m `div` 10)
-- sum larger than 19
predLargeSum = valid <&> (\n -> digitSum n >= 20) where
  digitSum m
    | m == 0 = 0
    | otherwise = (m `mod` 10) + digitSum (m `div` 10)
-- small set of random number
predRand1 = flip elem [4893,1063,3587,4281,3601,9561,0213,3054,4125,5731,1965,3492,1402,4376,4870,3825,0529,0481,1340,1327]

-- the list of all predicates (except predAll)
predicates :: [Int -> Bool]
predicates = [predOdd, predEven, predSmall, predLarge, predSmallSum, predLargeSum, predRand1]

-- test set
testsets :: [(Int, Int -> Bool)]
testsets = error "maybe implement it"

-- sanity checker for the test set
checkTestset :: Bool
checkTestset = and [valid n && p n | (n, p) <- testsets]

-- The Evaluator --

-- wrapped value of (failures, guess, time) where:
-- failure is the number of faiures
-- guess is the number of guess movements (including failed ones)
-- time is the total number of time consumped
data GameLog = GameLog {
  fails    :: Int,
  attempts :: Int,
  time     :: Int
} deriving (Eq, Ord, Show)

instance Monoid GameLog where
  mempty = GameLog 0 0 0
  (GameLog x1 y1 z1) `mappend` (GameLog x2 y2 z2) = GameLog (x1+x2) (y1+y2) (z1+z2)

type Game = StateT (StdGen, GameState, Int) (WriterT (Sum Int) IO)
  -- StateT (StdGen, GameState, Int) a   -- the last component is the count of guess
  -- WriterT (Sum Int) a                 -- log the time consumption
  -- IO a                                -- output

mytry :: IO a -> IO (Either SomeException a)
mytry = try

invoke :: NFData a => Int -> a -> IO (Maybe a)
invoke maxTime x = error "maybe implement it"

eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  de = blast e
  dx = blast x
  appeared = length $ filter (`elem` dx) de
  inPosition = length $ filter (uncurry (==)) $ zip (reverse de) (reverse dx)

-- run a test until failure
runOne :: Int -> Game Bool
runOne expected =
  do
    -- RandomNumberGenerator, GameState, Attempts
    (g, s, t) <- get
    error "maybe implement it"


-- testOne a=ID n=Expected p=Predicate "in that subset"
testOne :: Int -> Int -> (Int -> Bool) -> IO GameLog
testOne a n p = do
  putStrLn $ "[" ++ show a ++ "] Trying with " ++ show04 n
  let g = mkStdGen a
      s = initialize p
  ((success, (_, _, count)), t) <- runWriterT (runStateT (runOne n) (g, s, 0))
  putStrLn $ "success=" ++ show success
  putStrLn $ "takes " ++ show count ++ " guess and " ++ show (getSum t) ++ " milliseconds"
  return $ GameLog (ite success 0 1) count (getSum t)

-- run the benchmark
benchmark :: IO GameLog
benchmark = do
  putStrLn $ "Testing with " ++ show (length testsets) ++ " test cases"
  let results = mapM (\(a, (n,p)) -> testOne a n p) $ zip [1..] testsets
  subs <- results
  return $ mconcat subs

verbose :: IO ()
verbose = do
  log <- benchmark
  print log
  return ()
