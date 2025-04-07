-- Not trying to solve this exam with a working program, just trying to solve the problems
import Data.List
import Data.Foldable
import Data.Sequence as Seq 
import Data.Map as Map
import Data.Set as Set


data Coord = C Int Int 
    deriving (Show, Eq)
data Deplacement =  N | G | D | H | B | GH | GB | DH | DB
    deriving (Show, Eq)


bougeCoord:: Coord -> Deplacement -> Coord
bougeCoord (C x y) n | n == N = C x (y+1)
                     | n == G = C (x-1) y
                     | n == D = C (x+1) y
                     | n == H = C x (y-1)
                     | n == B = C x (y+1)
                     | n == GH = C (x-1) (y-1)
                     | n == GB = C (x-1) (y+1)
                     | n == DH = C (x+1) (y-1)
                     | n == DB = C (x+1) (y+1) 
-- Pour traiter correctement des Map dont les clefs sont des coordonn´ees (par exemple `a la question
-- 2.3), on a besoin de parcourir un ensemble de coordonn´ees selon un ordre particulier: on veut com-
-- mencer par la case la plus en haut, et la plus `a gauche (dans cet ordre), puis se diriger vers la
-- droite, et quand ca n’est plus possible, descendre `a une ligne suivante.
instance Ord Coord where 
    (C cx1 cy1) <= (C cx2 cy2) 
        | cy1 < cy2 = True
        | cy1 > cy2 = False
        | cy1 == cy2 = cx1 < cx2
-- parcourirMap :: Ord Coord => Map Coord a -> [(Coord, a)]
-- parcourirMap m = 
--     let coords = Data.List.sort (Map.keys m)
--     in Data.List.zip coords (Map.elems m)

prop_bougeCoordGaucheDroite :: Coord -> Deplacement -> Deplacement -> Bool
prop_bougeCoordGaucheDroite coord g d =
  bougeCoord (bougeCoord coord g) d == coord

prop_bougeCoordGaucheHaut :: Coord -> Deplacement -> Deplacement -> Bool
prop_bougeCoordGaucheHaut coord g h =
  bougeCoord (bougeCoord coord g) h == bougeCoord coord GH

-- preuve par induction
data Case = 
    Vide 
    | Terre 
    | Metale 
    | Entree 
    | Sortie
    deriving (Show, Eq)
data Niveau = Niveau {  
                        hNiveau :: Int ,
                        lNiveau :: Int ,
                        casesNiveau :: Map Coord Case
                     }
                        deriving (Eq)
prop_invariantNiveau :: Niveau -> Bool
prop_invariantNiveau niveau = 
    nbEntree niveau && nbSortie niveau && 
    niveauFermeParMetal niveau 
    && entreeAuDessusCaseVide niveau
    && sortieAudessusCaseMetale niveau && 
    coordValide niveau (C 0 0)

nbEntree :: Niveau -> Bool
nbEntree niveau = 
    let cases = Map.elems (casesNiveau niveau)
    in Data.Foldable.length (Data.List.filter (== Entree) cases) == 1
nbSortie :: Niveau -> Bool
nbSortie niveau = 
    let cases = Map.elems (casesNiveau niveau)
    in Data.Foldable.length (Data.List.filter (== Sortie) cases) == 1
    
niveauFermeParMetal :: Niveau -> Bool
niveauFermeParMetal niveau =
    let h = hNiveau niveau
        l = lNiveau niveau
        cases = casesNiveau niveau
        in Data.List.all
             (\(C x y, c) -> x == 0 || x == l || y == 0 || y == h || c == Metale)
             (Map.toList cases)
entreeAuDessusCaseVide :: Niveau -> Bool
entreeAuDessusCaseVide niveau = 
    let cases = casesNiveau niveau
        entree = Data.List.head (Data.List.filter (\(c, case') -> case' == Entree) (Map.toList cases))
        (C x y, _) = entree
    in (x, y) == (0, 1) && cases ! C x (y-1) == Vide
sortieAudessusCaseMetale :: Niveau -> Bool
sortieAudessusCaseMetale niveau = 
    let cases = casesNiveau niveau
        sortie = Data.List.head (Data.List.filter (\(c, case') -> case' == Sortie) (Map.toList cases))
        (C x y, _) = sortie
    in (x, y) == (lNiveau niveau, hNiveau niveau - 1) && cases ! C x (y+1) == Metale
coordValide :: Niveau -> Coord -> Bool
coordValide niveau (C x y) =
  0 <= x && x < lNiveau niveau &&
  0 <= y && y < hNiveau niveau

-- Voici une implémentation possible de Show et Read pour le type Niveau :

-- Haskell
instance Show Niveau where
  show niveau =
    let h = hNiveau niveau
        l = lNiveau niveau
        cases = casesNiveau niveau
    in unlines
      [ "Niveau " ++ show h ++ "x" ++ show l,
        concatMap
          (\(coord, case_) ->
            let (x, y) = coord
            in show x ++ "," ++ show y ++ ":" ++ show case_
          )
          (Map.toAscList cases),
      ]

instance Read Niveau where
  read s =
    let lines = words s
        h = read $ head lines
        l = read $ head $ tail lines
        cases = Map.fromList $ map transforme $ tail $ tail lines
    in Niveau {hNiveau = h, lNiveau = l, casesNiveau = cases}

transforme :: String -> (Coord, Case)
transforme line =
  let [xStr, yStr, caseStr] = words line
      x = read xStr
      y = read yStr
      case_ = read caseStr
  in (C x y, case_)

passable :: Coord -> Niveau -> Bool
passable coord niveau = 
    let cases = casesNiveau niveau
    in case cases ! coord of
        Vide -> True
        Terre -> True
        _ -> False
dure :: Coord -> Niveau -> Bool
dure coord niveau = 
    let cases = casesNiveau niveau
    in case cases ! coord of
        Metale -> True
        _ -> False


-- Partie 3 et 4 a definir