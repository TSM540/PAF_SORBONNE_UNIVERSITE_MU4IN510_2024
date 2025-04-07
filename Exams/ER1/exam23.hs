-- Map vide
-- empty :: Map k a
-- -- cherche la valeur associee a une clef
-- lookup :: Ord k => k -> Map k a -> Maybe a
-- -- fold sur les valeurs du Map
-- foldr :: (a -> b -> b) -> b -> Map k a -> b
-- -- fold sur les clefs et les valeurs du Map
-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
-- -- ajoute / remplace une association clef/valeur
-- insert :: Ord k => k -> a -> Map k a -> Map k a
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.Map as Map
import Data.Text as Text


-- ! Partie 1 
data Coord = C {
                    cx :: Int ,
                    cy :: Int
                }
                deriving (Show , Eq)

data Terrain = Herbe
                | Ressources Int
                | Eau
                 deriving (Show , Eq)

-- invariant Terrain 
prop_Inv_Terrain_Ressource_Positive :: Terrain -> Bool
prop_Inv_Terrain_Ressource_Positive (Ressources n) = n >= 0

newtype Carte = Carte {
            carte :: Map.Map Coord Terrain

}deriving (Show , Eq)
-- instance Ord Coord where 
--     (C cx1 cy1) <= (C cx2 cy2) 
--         | cy1 < cy2 = True
--         | cy1 > cy2 = False
--         | cy1 == cy2 = cx1 < cx2

instance Ord Coord => Ord Carte where
    (Carte c1) <= (Carte c2) = Map.keys c1 <= Map.keys c2

-- si deux cases de coordonn´ees (x1, y1) et (x2, y2) font partie de la carte, alors :
-- • soit toutes les cases des segments [(x1, y1), (x1, y2)] et [(x1, y2), (x2, y2)] font partie de la carte,
-- • soit toutes les cases des segments [(x1, y1), (x2, y1)] et [(x2, y1), (x2, y2)] font partie de la carte,
-- (cela force l’existence d’un ”chemin en coude” entre tout couple de points de la carte)
-- Donner un invariant pour le type Carte.


-- invariant Carte
prop_Inv_Carte :: Carte -> Coord -> Coord -> Bool
prop_Inv_Carte (Carte c) (C x1 y1) (C x2 y2) =
    let coords = Map.keys c
    in (not ((C x1 y1) Prelude.elem coords && (C x2 y2) Prelude.elem coords) || (if x1 == x2
                then
                    let yMin = min y1 y2
                        yMax = max y1 y2
                        coordsY = [C x y | y <- [yMin..yMax]]
                    in all (\coord -> coord `elem` coords) coordsY
            else
                let xMin = min x1 x2
                    xMax = max x1 x2
                    coordsX = [C x y | x <- [xMin..xMax]]
                in all (\coord -> coord `elem` coords) coordsX))

-- collecteCase :: Coord -> Int -> Carte -> (Int,Carte)
-- collecteCase C{cx= x, cy = y} r (Carte c) = undefined
--      --  extraire r ressource de la carte
--      -- retourner un couple (v, nc) v est le nombre de ressources effectivement extraite et nc  represente la carte après extraction 


collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
collecteCase (C x y) r (Carte carte) =
  case Map.lookup (C x y) $ carte of
    Nothing -> (0, carte)
    Just terrain ->
      case terrain of
        Herbe -> (0, carte)
        Ressources n ->
          if r <= n then
            (r, Carte $ Map.insert (C x y) Herbe (Map.delete (C x y) carte))
          else
            (n, Carte $ Map.insert (C x y) Herbe (Map.delete (C x y) carte))

-- prédcondition qui assure l'extracttion réussie 
prop_collecteCasePre :: Coord -> Int -> Carte -> Bool
prop_collecteCasePre (C x y) r (Carte carte) =
  case Map.lookup (C x y) $ carte of
    Nothing -> True
    Just terrain ->
      case terrain of
        Herbe -> True
        Ressources n -> r <= n

-- les test ?? je sais pas comment les faire
-- Partie 2
