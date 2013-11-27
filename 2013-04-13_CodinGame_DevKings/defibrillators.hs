import Text.ParserCombinators.ReadP
import Control.Monad (replicateM)
main = do
  long <- getFloat
  lat <- getFloat
  n <- readLn
  putStrLn . snd . foldl (act long lat) (1/0, undefined) =<< replicateM n getLine
act long0 lat0 a@(m,_) line | dist < m = (dist,name)
                            | otherwise = a
  where
    [_,name,_,_,_long,_lat] = fst $ head $ readP_to_S row line
    long = readFloat _long
    lat = readFloat _lat
    x = (long - long0) * cos ( (lat0 + lat) / 2 )
    y = lat - lat0
    dist = 6371 * sqrt (x ** 2 + y ** 2)
row = sepBy (munch (/= ';')) (char ';') >>= \r -> eof >> return r
readFloat = read . map toPoint
getFloat = fmap readFloat getLine
toPoint ',' = '.'
toPoint  c  =  c
