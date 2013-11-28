import Data.List (group)
import Data.Char (digitToInt)
main = do
  r <- readLn
  n <- readLn
  let l = iterate (concatMap conway . group) [r] !! (n-1)
  putStrLn $ unwords $ map show l
conway xs = [length xs, head xs]
