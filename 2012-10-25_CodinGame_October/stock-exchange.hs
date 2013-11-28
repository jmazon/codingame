main = print . solve . map read . tail . words =<< getContents
solve xs = minimum $ 0 : zipWith (-) xs (scanl1 max xs)
