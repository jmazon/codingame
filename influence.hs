import Data.Graph
import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

main = interact $ show . solve . map read . tail . words
pairs (x:y:zs) = (x,y) : pairs zs
pairs [] = []
solve xs = runST $ do
  let bs = (minimum xs, maximum xs)
      g = buildG bs $ pairs xs
      vs = topSort g
  a <- newArray bs 1 :: ST s (STUArray s Int Int)
  forM_ vs $ \v -> do
    l0 <- readArray a v
    forM_ (g!v) $ \w -> do
      l <- readArray a w
      writeArray a w (max l (l0+1))
  liftM maximum (getElems a)
