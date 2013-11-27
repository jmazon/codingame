import Data.List (sort)
main = print . minimum . map abs . diff . sort . map read . tail . words =<< getContents
diff l = zipWith (-) (tail l) l
