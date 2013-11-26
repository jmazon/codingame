import Control.Monad (replicateM)
import Data.List (sort)

main = do
    n <- readLn
    c <- readLn
    bs <- replicateM n readLn
    putStrLn $ maybe "IMPOSSIBLE" (unlines . map show) $ solve n c (sort bs) []

solve _ c [] a | c > 0     = Nothing
               | otherwise = Just (reverse a)
solve n c (b:bs) a = solve (n-1) (c-s) bs (s:a)
  where s = min b (c `div` n)
