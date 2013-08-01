import Data.Ord      (comparing)
import Data.List     (permutations,isPrefixOf)
import Control.Monad (replicateM,liftM,foldM)
main = print                          .
       minimum                        .
       map length                     .
       concatMap (foldM superpose "") .
       permutations                  =<<
       flip replicateM getLine       =<<
       readLn
superpose s1 s2 = merge s1 s2 =<< [-length s1..length s2]
merge (a:as) bs o | o < 0 = liftM (a:) (merge as bs (o+1))
merge as (b:bs) o | o > 0 = liftM (b:) (merge as bs (o-1))
merge as bs 0 | as `isPrefixOf` bs = return bs
              | bs `isPrefixOf` as = return as
              | otherwise = []
