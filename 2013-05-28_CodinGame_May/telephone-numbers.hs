import Data.List (sort)
main = print . solve . tail . lines =<< getContents
solve ts = length $ concat $ head ts' : zipWith stripPrefix ts' (tail ts')
  where ts' = sort ts
stripPrefix (a:as) (b:bs) | a == b = stripPrefix as bs
stripPrefix    as    []            = []
stripPrefix   _       bs           = bs
