import Control.Monad (replicateM,liftM)
import Control.Arrow (first)
import Data.List (find)

data Phrase = Phrase Sujet Verbe (Maybe GN)
data Sujet = PP Personne Genre Nombre | Sujet GN
data Genre = Masculin | Feminin deriving Enum
data Nombre = Singulier | Pluriel deriving Enum
data Personne = Premiere | Deuxieme | Troisieme deriving Enum
data GN = GN Nombre Article [Adjectif] Nom [Adjectif]
data Article = Indefini | Defini
data Nom = Nom { nomGenre :: Genre, nomDesinences :: [String] }
newtype Adjectif = Adjectif { unAdj :: [String] }
newtype Verbe = Verbe [String]
data Dict = Dict Verbe [Adjectif] [Nom]

main = do
    v <- fmap (Verbe . words) getLine
    a <- readLn
    as <- replicateM a $ liftM (Adjectif . words) getLine
    n <- readLn
    ns <- replicateM n $ liftM (readNom . words) getLine
    p <- fmap words getLine
    let p' = head $ phrase (Dict v as ns) p
    putStrLn $ unwords $ correctPhrase p'

readNom (g:ns) = let g' = case g of
                            "masculin" -> Masculin
                            "feminin" -> Feminin
                 in Nom g' ns

opt parser ts = liftP Just (parser ts) ++ return (Nothing,ts)
many parser ts = many1 parser ts ++ return ([],ts)
many1 parser ts = do
  (r,ts') <- parser ts
  liftP (r:) (many parser ts')
liftP p = liftM (first p)

phrase (Dict v as ns) ts = do
  (s,ts') <- sujet as ns ts
  (v,ts'') <- verbe v ts'
  (c,[]) <- opt (gn as ns) ts''
  return (Phrase s v c)
correctPhrase (Phrase s v c) =
    correctSujet s ++ correctVerbe p n v : maybe [] correctGN c
  where n = nombre s
        p = personne s

sujet as ns ts = pp ts ++ liftP Sujet (gn as ns ts)
correctSujet (PP Premiere  Masculin Singulier) = ["je"]
correctSujet (PP Deuxieme  Masculin Singulier) = ["tu"]
correctSujet (PP Troisieme Masculin Singulier) = ["il"]
correctSujet (PP Troisieme Feminin  Singulier) = ["elle"]
correctSujet (PP Premiere  Masculin Pluriel  ) = ["nous"]
correctSujet (PP Deuxieme  Masculin Pluriel  ) = ["vous"]
correctSujet (PP Troisieme Masculin Pluriel  ) = ["ils"]
correctSujet (PP Troisieme Feminin  Pluriel  ) = ["elles"]
correctSujet (Sujet gn) = correctGN gn
personne (PP p _ _) = p
personne (Sujet _) = Troisieme
nombre (PP _ _ n) = n
nombre (Sujet (GN n _ _ _ _)) = n

pp ("je"   :ts) = return (PP Premiere  Masculin Singulier,ts)
pp ("tu"   :ts) = return (PP Deuxieme  Masculin Singulier,ts)
pp ("il"   :ts) = return (PP Troisieme Masculin Singulier,ts)
pp ("elle" :ts) = return (PP Troisieme Feminin  Singulier,ts)
pp ("nous" :ts) = return (PP Premiere  Masculin Pluriel,  ts)
pp ("vous" :ts) = return (PP Deuxieme  Masculin Pluriel,  ts)
pp ("ils"  :ts) = return (PP Troisieme Masculin Pluriel,  ts)
pp ("elles":ts) = return (PP Troisieme Feminin  Pluriel,  ts)
pp _ = []

gn as ns ts = do
  (nb,a,ts') <- article ts
  (a1,ts'') <- many (adjectif as) ts'
  (n,ts''') <- nom ns ts''
  (a2,ts'''') <- many (adjectif as) ts'''
  return (GN nb a a1 n a2,ts'''')
correctGN (GN nb a a1 (Nom g ns) a2) =
    correctArticle g nb a :
    map (correctAdjectif g nb) a1 ++
    (ns !! fromEnum nb) :
    map (correctAdjectif g nb) a2

article ("un" :ts) = return (Singulier,Indefini,ts)
article ("une":ts) = return (Singulier,Indefini,ts)
article ("des":ts) = return (Pluriel,  Indefini,ts)
article ("le" :ts) = return (Singulier,Defini,  ts)
article ("la" :ts) = return (Singulier,Defini,  ts)
article ("les":ts) = return (Pluriel,  Defini,  ts)
article _ = []
correctArticle Masculin Singulier Indefini = "un"
correctArticle Feminin  Singulier Indefini = "une"
correctArticle _        Pluriel   Indefini = "des"
correctArticle Masculin Singulier Defini   = "le"
correctArticle Feminin  Singulier Defini   = "la"
correctArticle _        Pluriel   Defini   = "les"

adjectif as (t:ts) = case find (elem t . unAdj) as of
                       Nothing -> []
                       Just a  -> return (a,ts)
adjectif _ _ = []
correctAdjectif g n (Adjectif as) = as !! (fromEnum n + 2*fromEnum g)

nom ns (t:ts) = case find (elem t . nomDesinences) ns of
                  Nothing -> []
                  Just n  -> return (n,ts)

verbe v@(Verbe vs) (t:ts) | elem t vs = return (v,ts)
                          | otherwise = []
correctVerbe p n (Verbe vs) = vs !! (3*fromEnum n + fromEnum p)
