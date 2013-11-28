import Data.List (foldl1',unfoldr,elemIndex)
import Control.Monad (replicateM)
main = do
    [l,h] <- fmap (map read . words) getLine
    ds <- fmap (parseDigits l) (replicateM h getLine)
    n1 <- readNumber ds h
    n2 <- readNumber ds h
    _o <- getLine
    let op = case _o of
               "+" -> (+)
               "-" -> (-)
               "*" -> (*)
               "/" -> div
    putStr $ unlines $ concatMap (ds!!) $ encode $ op n1 n2
parseDigits l buf = map extract [0..19]
  where extract i = map (take l . drop (l*i)) buf
readNumber ds h = do
    d <- readLn
    ds <- replicateM (d `div` h) (fmap (identifyDigit ds) (readDigit h))
    return $ foldl1' (\a b -> 20*a + b) ds
readDigit h = replicateM h getLine
identifyDigit ds d = n where Just n = elemIndex d ds
encode = zero . reverse . unfoldr go
  where go 0 = Nothing
        go n = Just (r,q) where (q,r) = n `divMod` 20
        zero [] = [0]
        zero xs = xs
