import Data.List
import System.Environment
import System.IO

data Prediction = Prediction { start :: Int, stop :: Int, score :: Int }

instance Show Prediction where
  show p = "(" ++ show (start p) ++ ", " ++ show (stop p) ++ ", " ++ show (score p) ++ ")"

instance Eq Prediction where
  a == b = (start a == start b) && (stop a == stop b) && (score a == score b)

instance Ord Prediction where
  compare a b = start a `compare` stop b

getNLines :: Handle -> Int -> IO [String]
getNLines h n = mapM (const $ hGetLine h) [1..n]

readGattacaFormat :: Handle -> IO [Prediction]
readGattacaFormat h = do
  skipDNA h
  readPredictions h

skipDNA :: Handle -> IO ()
skipDNA h = do
  n         <- hGetLine h
  getNLines h $ ceiling (read n / 80)
  return ()

readPredictions :: Handle -> IO [Prediction]
readPredictions h = do
  g                <- hGetLine h
  prediction_lines <- getNLines h $ read g
  return $ map parsePrediction prediction_lines

parsePrediction :: String -> Prediction
parsePrediction p = case map read (words p) of (s : t : c : []) -> Prediction { start = s, stop = t, score = c }

optimalScore :: [Prediction] -> Int
optimalScore = maximum . map scoreSet . filter nonOverlapping . subsequences . sort

scoreSet :: [Prediction] -> Int
scoreSet = sum . map score

nonOverlapping :: [Prediction] -> Bool
nonOverlapping []       = False
nonOverlapping (_:[])   = True
nonOverlapping (p:ps) = (stop p < start (head ps)) && nonOverlapping ps

main :: IO ()
main = do
  input_filename:args <- getArgs
  predictions <- withFile input_filename ReadMode readGattacaFormat
  print $ optimalScore predictions
