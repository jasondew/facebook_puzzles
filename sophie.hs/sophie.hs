import Data.Array.Unboxed
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.IO
import Text.Printf

type Location = String
data LocationProbability = LocationProbability { location :: Location, probability :: Double }
data Distance = Distance { from :: String, to :: String, time :: Int }

instance Show LocationProbability where
  show p = (show . location $ p) ++ ":" ++ (show . probability $ p)

instance Show Distance where
  show d = (show . from $ d) ++ " -> " ++ (show . to $ d) ++ ": " ++ (show . time $ d)

instance Eq Distance where
  d1 == d2 = time d1 == time d2

instance Ord Distance where
  d1 `compare` d2 = (time d1) `compare` (time d2)

readSophieFormat :: Handle -> IO ([LocationProbability], [Distance])
readSophieFormat h = do
  m_string          <- hGetLine h
  probability_lines <- mapM (const $ hGetLine h) [1..read m_string]
  let probabilities = map parseLocationProbability probability_lines

  c_string          <- hGetLine h
  distance_lines    <- mapM (const $ hGetLine h) [1..read c_string]
  let distances = map parseDistance distance_lines

  return (probabilities, distances)

parseLocationProbability :: String -> LocationProbability
parseLocationProbability p = case words p of
                    (l : p : []) -> LocationProbability { location = l, probability = read ("0" ++ p) }

parseDistance :: String -> Distance
parseDistance d = case words d of
                    (f : t : e : []) -> Distance { from = f, to = t, time = read e }

minExpectedTime :: [LocationProbability] -> [Distance] -> Double
minExpectedTime lps ds = f lps (location . head $ lps) 0 0.0
  where f []                                                        _               _             expectation = expectation
        f (LocationProbability {location = l, probability = p}:lps) currentLocation distanceSoFar expectation = f lps l newDistance newExpectation
          where newDistance    = distanceSoFar + (currentLocation `distanceTo` l)
                newExpectation = expectation + (p * (fromIntegral newDistance))
        distanceTo s t = case Map.lookup (s,t) $ paths of
                           Just d  -> d
                           Nothing -> 0
        paths = shortestPaths locations ds
        locations = map location lps

-- Floyd-Warshall
-- prepopulate d[0][i][j] with distance from i to j, infinity if no direct path exists
-- iterate k from 1 to # vertices; d[k][i][j] = min d[k-1][i][j], (d[k-1][i][k] + d[k-1][k][j])
shortestPaths :: [Location] -> [Distance] -> Map (Location, Location) Int
shortestPaths locations distances = floydWarshall locations $ undirectedMap distances

floydWarshall :: [Location] -> Map (Location, Location) Int -> Map (Location, Location) Int
floydWarshall locations = f (locationPairs locations) locations
  where f pairs []     m = m
        f pairs (l:ls) m = f pairs ls $ floydWarshallIteration l pairs m

floydWarshallIteration :: Location -> [(Location, Location)] -> Map (Location, Location) Int -> Map (Location, Location) Int
floydWarshallIteration l pairs m = foldl f m pairs
  where f m p = Map.insertWith min p (indirectPath p l m) m

-- FIXME: need to use a monoid here instead of arbitrary upper bound
indirectPath :: (Location, Location) -> Location -> Map (Location, Location) Int -> Maybe Int
indirectPath (i, j) l m = case Map.lookup (i, l) m of
                          Just d1 -> case Map.lookup (l, j) m of
                                      Just d2 -> d1 + d2
                                      Nothing -> 1000000
                          Nothing -> 1000000
  
locationPairs :: [Location] -> [(Location, Location)]
locationPairs locations = f locations []
  where f []     pairs = pairs
        f (l:ls) pairs = f ls $ pairs ++ zip (repeat l) ls

undirectedMap :: [Distance] -> Map (Location, Location) Int
undirectedMap = f Map.empty
  where f m []                                         = m
        f m (Distance {from = s, to = t, time = n}:ds) = Map.insert (t, s) n $ Map.insert (s, t) n $ f m ds

findable :: [LocationProbability] -> Bool
findable lps = sum == 1.0
  where sum = foldl (\acc lp -> acc + probability lp) 0.0 lps

main :: IO ()
main = do
  (input_filename:_)                  <- getArgs
  (location_probabilities, distances) <- withFile input_filename ReadMode readSophieFormat

  case findable location_probabilities of
    True  -> printf "%.2f\n" $ minExpectedTime location_probabilities distances
    False -> putStrLn "-1.00"
