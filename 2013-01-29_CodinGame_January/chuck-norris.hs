import Data.Bits (testBit)
import Data.List (group)
main = putStrLn . unwords . map encode . concatMap bits .
       group . concatMap (unBits . fromEnum) =<< getLine
unBits c = map (testBit c) [6,5..0]
bits xs = [if head xs then 1 else 0,length xs]
encode n = replicate (if n == 0 then 2 else n) '0'
