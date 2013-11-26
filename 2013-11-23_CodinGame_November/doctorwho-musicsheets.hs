import Data.Array
import Data.List (inits,sort,sortBy,groupBy)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Control.Monad (guard)
import Control.Arrow (first,(&&&))

main = do
    [w,h] <- fmap (map read . words) getLine
    score <- fmap (listArray ((1,1),(h,w)) . decode . words) getLine
    let lines = findLines score
        score' = clearLedger score lines
        times = findNotes score'
        seminotes = map (identifyNote score' (genMask h lines)) times
        notes = map (first (pitch (grid lines))) seminotes
    putStrLn $ unwords $ map showNote notes

decode [] = []
decode (c:n:ws) = replicate (read n) (case c of "B" -> True; "W" -> False)
                  ++ decode ws

findLines s = head $ catMaybes $ map validLines $ inits candidates
  where
    (h,w) = snd (bounds s)
    hist = [ length (filter id (row i)) | i <- [1..h] ]
    row i = [ s ! (i,j) | j <- [1..w] ]
    candidates = map snd $ sortBy (flip compare) (zip hist [1..])

validLines _ls = do
  let ls = sort _ls
      ls' = groupContiguous ls
  guard (length ls' == 5)
  let w = fst (head ls') - snd (head ls')
  guard (all (== w) (map (uncurry (-)) ls'))
  let d = fst (head (tail ls')) - fst (head ls')
  guard (map fst ls' == take 5 (iterate (+d) (fst (head ls'))))
  return ls'

groupContiguous xs = map (snd . head &&& snd . last) ys'
  where
    ns = zipWith (-) xs [0..]
    ys = zip ns xs
    ys' = groupBy ((==) `on` fst) ys

findNotes s = notes
  where
    (h,w) = snd (bounds s)
    hist = listArray (1,w) [ length (filter id (col j)) | j <- [1..w] ]
    col j = [ s ! (i,j) | i <- [1..h] ]
    cut = 3 * maximum (elems hist) `div` 4
    stems = groupContiguous $ map fst $ filter ((>= cut) . snd) $ assocs hist
    notes = map extractNote stems
    extractNote (l,r) = minMax a end
      where
        (rest,a,d) | (hist!(l-1)) > (hist!(r+1)) = (hist!(r+1),l-1,-1)
                   | (hist!(l-1)) < (hist!(r+1)) = (hist!(l-1),r+1,1)
                   | otherwise = error "Ugh"
        end = fst $ last $
              takeWhile ((/= rest) . snd) $
              map (id &&& (hist!)) $
              iterate (+d) a

minMax a b = (min a b,max a b)

identifyNote score mask (l,r) = (center,score!(center,(l+r) `div` 2))
  where
    (h,w) = snd (bounds score)
    scan x = map fst $ filter snd $ zip [1..] $
             zipWith (/=) mask [ score!(y,x) | y <- [1..h] ]
    dots = concatMap scan [l..r]
    center = (sum dots + (length dots `div` 2)) `div` (length dots)

genMask h ls = elems $ accumArray (||) False (1,h) $
               flip zip (repeat True) $
               concatMap (uncurry enumFromTo) ls

grid ((h1,h2):(h3,_):_) = ((h1+h2) `div` 2,h3-h1)

pitch (h0,i) h = case (2*h - 2*h0 + i `div` 2) `div` i of
                   -1 -> 'G'
                   0 -> 'F'
                   1 -> 'E'
                   2 -> 'D'
                   3 -> 'C'
                   4 -> 'B'
                   5 -> 'A'
                   6 -> 'G'
                   7 -> 'F'
                   8 -> 'E'
                   9 -> 'D'
                   10 -> 'C'

showNote (p,q) = p : if q then "Q" else "H"

clearLedger score ((h1,h2):(h3,_):_) = score // concatMap clean (filter scan [1..w])
  where
    (h,w) = snd (bounds score)
    (a,b) = (h1 + 5*(h3-h1),h2 + 5*(h3-h1))
    scan j = (not $ score!(a-1,j)) &&
             (not $ score!(b+1,j)) &&
             and [ score!(i,j) | i <- [a..b] ]
    clean j = [ ((i,j),False) | i <- [a..b] ]
