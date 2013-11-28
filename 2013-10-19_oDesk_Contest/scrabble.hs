import Control.Monad (replicateM)
import Control.Arrow ((&&&))
import Data.List (sort)
main = do
    n <- readLn
    words <- replicateM n getLine
    letters <- fmap sort getLine
    let candidates = map (id &&& score) $ filter (contains letters) words
        v = maximum (map snd candidates)
    putStrLn $ fst $ head $ filter ((== v) . snd) candidates
contains ls = go ls . sort
  where go (l:ls) (w:ws) | w < l = False
                         | w > l = go ls (w:ws)
                         | otherwise = go ls ws
        go _ [] = True
        go _ _ = False
score = sum . map l
  where l 'e' = 1
        l 'a' = 1
        l 'i' = 1
        l 'o' = 1
        l 'n' = 1
        l 'r' = 1
        l 'l' = 1
        l 't' = 1
        l 'l' = 1
        l 's' = 1
        l 'u' = 1
        l 'd' = 2
        l 'g' = 2
        l 'b' = 3
        l 'c' = 3
        l 'm' = 3
        l 'p' = 3
        l 'f' = 4
        l 'h' = 4
        l 'v' = 4
        l 'w' = 4
        l 'y' = 4
        l 'k' = 5
        l 'j' = 8
        l 'x' = 8
        l 'q' = 10
        l 'z' = 10
