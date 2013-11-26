import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Monoid ((<>))
main = print . solve . map read . tail . words =<< getContents
solve [] = 0
solve xs = minimumBy (comparing abs <> flip compare) xs
