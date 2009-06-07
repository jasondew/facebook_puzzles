import Data.List
import System.Environment
import System.IO

data Prediction = Prediction { start :: Int, stop :: Int, score :: Int }

instance Show Prediction where
  show p = " (" ++ (show $ start p) ++ "-" ++ (show $ stop p) ++ "):" ++ (show $ score p) ++ " "

instance Eq Prediction where
  a == b = (start a == start b) && (stop a == stop b) && (score a == score b)

instance Ord Prediction where
  compare a b = start a `compare` start b

readGattacaFormat :: Handle -> IO [Prediction]
readGattacaFormat h = do
  n_string <- hGetLine h
  let n = ceiling (read n_string / 80)
  mapM_ (const $ hGetLine h) [1..n]

  g_string         <- hGetLine h
  prediction_lines <- mapM (const $ hGetLine h) [1..(read g_string)]
  return $ map parsePrediction prediction_lines

parsePrediction :: String -> Prediction
parsePrediction p = case map read (words p) of
                    (s : t : c : []) -> Prediction { start = s, stop = t, score = c }

optimalScore :: [Prediction] -> Int
optimalScore = const 42

overlaps :: Prediction -> Prediction -> Bool
p `overlaps` q = ((start p <= start q) && (start q <= stop p)) || ((start p <= stop q) && (stop q <= stop p))

scoreSet :: [Prediction] -> Int
scoreSet = sum . map score

main :: IO ()
main = do
  (input_filename:_) <- getArgs
  predictions        <- withFile input_filename ReadMode readGattacaFormat

  putStrLn $ "n = " ++ (show . length $ predictions)
  putStrLn $ "predictions = " ++ (show predictions)

  let ss = map (\s -> ((sum $ map score s), s)) . filter nonOverlapping . subsequences $ predictions
  let max = maximum $ map (scoreSet . snd) ss
  let answers = filter (\s -> fst s == max) ss

  putStrLn $ "max = " ++ (show max)
  mapM_ (\answer -> putStrLn $ "answer = " ++ (show answer)) answers

  putStrLn . show $ optimalScore predictions
