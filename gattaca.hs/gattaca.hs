import Data.List
import Data.Maybe
import System.Environment
import System.IO

data Prediction = Prediction { start :: !Int, stop :: !Int, score :: !Int }

instance Show Prediction where
  show p = "[" ++ (show $ start p) ++ ", " ++ (show $ stop p) ++ ", " ++ (show $ score p) ++ "]"

instance Eq Prediction where
  a == b = (start a == start b) && (stop a == stop b) && (score a == score b)

instance Ord Prediction where
  compare a b = stop a `compare` stop b

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
optimalScore = optimalScore' . sort where
  optimalScore' (first:rest) = f [(stop first, score first)] (stop first) rest
                               where f maximums _         []     = snd . head $ maximums
                                     f maximums last_stop (p:ps) = case attempt > (snd . head $ maximums) of
                                                                     True  -> f ((stop p, attempt) : maximums) (stop p) ps
                                                                     False -> f maximums (stop p) ps
                                                                   where attempt = score p + (best_less_than . start $ p)
                                                                         best_less_than n = case find (\(point, _) -> point < n) maximums of
                                                                                              Nothing -> 0
                                                                                              Just q  -> snd q

main :: IO ()
main = do
  (input_filename:_) <- getArgs
  predictions        <- withFile input_filename ReadMode readGattacaFormat

  putStrLn . show $ optimalScore predictions
