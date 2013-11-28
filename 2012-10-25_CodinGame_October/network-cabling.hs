import Data.List (sort)
main = print . solve . pairs . map read . tail . words =<< getContents
pairs (x:y:zs) = (x,y) : pairs zs
pairs [] = []
solve ps = maximum xs - minimum xs + sum (map (abs . subtract y) ys)
  where
    (xs,ys) = unzip ps
    y = sort ys !! (length ys `div` 2)
