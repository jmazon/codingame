import           Data.Array          (listArray, (!))
import           Data.List           (foldl')
import qualified Data.IntMap as M    (empty,insert,lookup)
import           Control.Monad       (liftM, liftM2, replicateM)
-- WTF CodinGame
-- import        Control.Monad.State (evalState,get,modify)

toBool '.' = False
toBool '-' = True
morse = listArray ('A','Z') $ map (map toBool)
        [ ".-",   "-...", "-.-.", "-.."  
        , ".",    "..-.", "--.",  "...." 
        , "..",   ".---", "-.-",  ".-.." 
        , "--",   "-.",   "---",  ".--." 
        , "--.-", ".-.",  "...",  "-"    
        , "..-",  "...-", ".--",  "-..-" 
        , "-.--", "--.." ]
data Dic = Bin !Int !Dic !Dic | Tip deriving Show
dicInsert d = go d . concatMap (morse!)
    where go Tip w = go (Bin 0 Tip Tip) w
          go (Bin i d1 d2) [] = Bin (i+1) d1 d2
          go (Bin i d1 d2) (c:cs) = if c then Bin i d1 (go d2 cs)
                                         else Bin i (go d1 cs) d2
count dict s = evalState (go dict s 0) M.empty where
  go (Bin i d1 d2) s@(c:cs) p = liftM2 (+)
        ( if c then go d2 cs $! p+1 else go d1 cs $! p+1 )
        ( if i > 0 then liftM (*i) (newWord s p) else return 0 )
  go (Bin i  _  _)   []   _ = return i
  go Tip             _    _ = return 0
  newWord s p = do v <- liftM (M.lookup p) get
                   case v of Just v' -> return v'
                             Nothing -> do v' <- go dict s p
                                           modify (M.insert p v')
                                           return v'
main = do s <- liftM (map toBool) getLine
          n <- readLn
          dict <- liftM (foldl' dicInsert Tip) (replicateM n getLine)
          print (count dict s)

-- ridiculous reimplementation of Control.Monad.State
newtype State s a = State { runState :: s -> (a,s) }
evalState m = fst . runState m
get = State $ \s -> (s,s)
put s = State $ \_ -> ((),s)
modify f = get >>= put . f
instance Monad (State s) where
    return a = State $ \s -> (a,s)
    m >>= f = State $ \s -> let (a,s') = runState m s in runState (f a) s'
