import Data.Maybe
import System.Environment
import System.IO

divides :: Int -> Int -> Bool
divides a b = a `mod` b == 0

hoppityHop :: Int -> [String]
hoppityHop n = mapMaybe hoppity [1..n]

hoppity :: Int -> Maybe String
hoppity n | n `divides` 15 = Just "Hop"
hoppity n | n `divides` 5  = Just "Hophop"
hoppity n | n `divides` 3  = Just "Hoppity"
hoppity _ = Nothing

main :: IO ()
main = do
  input_filename:args <- getArgs
  contents <- withFile input_filename ReadMode hGetLine
  mapM_ putStrLn (hoppityHop . read $ contents)
