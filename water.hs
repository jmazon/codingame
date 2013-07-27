import Data.Array
import Control.Monad
import Data.Array.ST
import Control.Monad.ST
import Control.Arrow

isWater 'O' = True
isWater '#' = False

readPair = getLine >>= return . ((!!1) &&& (!!0)) . map read . words
main = do
  l <- readLn
  h <- readLn
  g <- fmap (listArray ((0,0),(h-1,l-1)) . map isWater . concat) (replicateM h getLine)
  n <- readLn
  ps <- replicateM n readPair
  let as = runST $ do
             parent <- newListArray (bounds g) (indices g) :: ST s (STArray s (Int,Int) (Int,Int))
             size <- newArray (bounds g) 1 :: ST s (STUArray s (Int,Int) Int)
             forM_ (assocs g) $ \((i,j),c) -> when c $ do
               when (i > 0 && g!(i-1,j)) $ union parent size (i,j) (i-1,j)
               when (j > 0 && g!(i,j-1)) $ union parent size (i,j) (i,j-1)
             forM ps $ \p -> if (g!p) then find parent p >>= readArray size
                             else return 0
  mapM_ print as

{-# INLINE union #-}
union parent size x y = do
  xRoot <- find parent x
  yRoot <- find parent y
  when (xRoot /= yRoot) $ do
    xSize <- readArray size xRoot
    ySize <- readArray size yRoot
    if xSize < ySize
      then do
        writeArray parent xRoot yRoot
        writeArray size yRoot $! ySize + xSize
      else do
        writeArray parent yRoot xRoot
        writeArray size xRoot $! ySize + xSize

{-# INLINE find #-}
find parent = go
    where go x = do
            xParent <- readArray parent x
            when (xParent /= x) $ go xParent >>= writeArray parent x
            readArray parent x
