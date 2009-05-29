import Control.Monad
import Data.List
import System.Environment
import System.IO

type DNA = String
data Prediction = Prediction { start :: Int, stop :: Int, score :: Int }

instance Show Prediction where
  show p = "(" ++ show (start p) ++ ", " ++ show (stop p) ++ ", " ++ show (score p) ++ ")"

instance Eq Prediction where
  a == b = (start a == start b) && (stop a == stop b) && (score a == score b)

instance Ord Prediction where
  compare a b = start a `compare` stop b

getNLines :: Handle -> Int -> IO [String]
getNLines h n = mapM (const $ hGetLine h) [1..n]

readGattacaFormat :: Handle -> IO (DNA, [Prediction])
readGattacaFormat h = do
  dna         <- readDNA h
  predictions <- readPredictions h
  return (dna, predictions)

readDNA :: Handle -> IO DNA
readDNA h = do
  n         <- hGetLine h
  dna_lines <- getNLines h $ ceiling (read n / 80)
  return $ concat dna_lines

readPredictions :: Handle -> IO [Prediction]
readPredictions h = do
  g                <- hGetLine h
  prediction_lines <- getNLines h $ read g
  return $ map parsePrediction prediction_lines

parsePrediction :: String -> Prediction
parsePrediction p = listToPrediction $ map read (words p)
                    where listToPrediction (s : t : c : []) = Prediction { start = s, stop = t, score = c }

optimalScore :: [Prediction] -> Int
optimalScore = maximum . (map scoreSet) . (filter nonOverlapping) . (map sort) . subsequences

scoreSet :: [Prediction] -> Int
scoreSet = sum . map score

nonOverlapping :: [Prediction] -> Bool
nonOverlapping []       = True
nonOverlapping (_:[])   = True
nonOverlapping (a:b:ps) = (stop a < start b) && nonOverlapping (b:ps)

main :: IO ()
main = do
  input_filename:args <- getArgs
  contents <- withFile input_filename ReadMode readGattacaFormat
  let dna = fst contents
      predictions = snd contents

  putStrLn . show $ optimalScore predictions
