import Guess
-- import System.Random.Shuffle
import System.Random
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.Printf
import Data.Time
import System.IO

show04 :: Int -> String
show04 = printf "%04d"

valid :: Int -> Bool
valid n = 120 <= n && n <= 9999 && noDup n where noDup x = length (Data.List.nub (show04 x)) == 4

type Game = StateT (StdGen, GameState) (WriterT (Sum Int) IO)
  -- StateT (StdGen, GameState) a   -- maintain the change of states
  -- WriterT (Sum Int) a            -- count the nunber of guess
  -- IO a                           -- output

space :: [Int]
space = (filter valid [1000..2000])

spaceLen :: Int
spaceLen = length space


eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

csi :: IO Integer
csi = (`div` 1000000000000) <$> diffTimeToPicoseconds <$> utctDayTime <$> getCurrentTime

-- randomT :: RandomGen g => g -> GameState -> Int
-- randomT g (GS lis sl _) = let randomIndex = fst $ randomR (0, sl - 1) g in
--     lis !! randomIndex

play :: Handle -> Int -> Game ()
play h answer = do
  (g, s) <- get
  let (g1, g2) = split g
      (x, s') = guess g1 s
      (r1, r2) = eval answer x
      s'' = refine (x,s') (r1, r2)
  put (g2, s'')
  tell (Sum 1)
  -- liftIO $ putStrLn $ "you guess " ++ (show x) ++ " -> (" ++ (show r1) ++ ", " ++ (show r2) ++ ")"
  liftIO ((show <$> csi) >>= (\s -> hPutStrLn h s))
  when (x /= answer) (play h answer)


randomNumber :: StdGen -> Int
randomNumber g = let randomIndex = fst $ randomR (0, spaceLen - 1) g in
    space !! randomIndex

oneGuess :: Handle -> StdGen -> Int -> IO Int
oneGuess h g i = do
    let s = initialize valid
    (_, r) <- runWriterT (runStateT (play h i) (g, s))
    -- (_, r) <- runWriterT (runStateT (play i) (g, s))
    -- out <- putStrLn $ "it takes " ++ (show (getSum r)) ++ " guesses"
    -- putStrLn $ ("random is " ++ (show (space !! (fst $ randomR (0, 4535) g ))))
    putStrLn (show i)
    hPutStrLn h (show (getSum r) ++ "," ++ (show i))
    return $ getSum r

tspace :: [Int]
tspace = [4378, 4938, 5843, 7195, 6791, 9421, 9532, 9820, 9863  ]

go :: Handle -> [Int] -> IO ()
go h [] = putStrLn "end"
go h (x:xs) = do
    times <- oneGuess h (mkStdGen x) x
    go h xs

main :: IO ()
main = do
    outh <- openFile "seg.txt" WriteMode
    go outh space
    hClose outh
-- main = go (length need8)
  -- putStrLn $ "I won't tell you 9527 is the number"
  -- putStrLn $ "I won't tell you 9527 is the number"
  -- let g = mkStdGen 0
  --     s = initialize valid
  -- (_, r) <- runWriterT (runStateT play (g, s))
  -- -- putStrLn $ "Bingo!"
  -- putStrLn $ "it takes " ++ (show (getSum r)) ++ " guesses"
