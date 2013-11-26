import Data.Array (listArray,(!))
import Data.Char (isAlpha,toUpper)
import Control.Monad (replicateM)
main = do
    l <- readLn
    h <- readLn
    t <- getLine
    buf <- fmap (listArray ((1,1),(h,27*l)) . concat) (replicateM h getLine)
    let scan i c = [ buf!(i,b*l+o) | o <- [1..l] ]
          where
            b | isAlpha c = fromEnum (toUpper c) - fromEnum 'A'
              | otherwise = 26
    putStr $ unlines $ map (concat . flip map t . scan) [1..h]
