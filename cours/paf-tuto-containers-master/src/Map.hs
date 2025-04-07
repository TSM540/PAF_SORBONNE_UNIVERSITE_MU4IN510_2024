
module Map where

-- on va utiliser des chaînes de type Text
import Data.Text (Text)
import qualified Data.Text as Text

{-

# Les tables associatives (maps)

Les tables d'associations sont des structures très utiles
en programmation.

Il s'agit d'une structure composée d'un ensemble
d'associations (clé, valeur) où chaque clé (unique) possède
une valeur associée.

Dans le type  Map k v   où k est le type des clé et v le type des valeurs,
le type k doit être ordonné, c'est à dire instance de Ord.
On peut généralement dériver cette contrainte (cf. ci-dessous)

La bibliothèque containers propose une implémentation
d'une version immutable efficace dans le module Data.Map

On commence donc par les imports suivants :

-}

import Data.Map (Map , (!?))  -- on a aussi importé l'opérateur de "lookup"
-- il n'y a pas de constructeurs explicites, l'implémentation est "cachée"
import qualified Data.Map as Map

-- On utilise aussi quelques folds génériques
import Data.Foldable (foldr)

{-

## Constuction des maps

Sans surprise, la construction des maps explicites se fait avec fromList

-}

-- >>> :t Map.fromList
-- Map.fromList :: Ord k => [(k, a)] -> Map k a

myMap :: Map Text Integer
myMap = Map.fromList [("a", 1), ("b", 2), ("c", 3), ("d", 4)]

-- Dans le type de fromList, le Ord k signifie que le type k doit être ordonné

-- Exemple de type ordonné
data Nat =
  Z
  | S Nat
  deriving (Show, Eq, Ord)

-- >>> Z <= S Z
-- True

-- >>> S Z <= S (S Z)
-- True

natMap :: Map Nat Text
natMap = Map.fromList [(Z, "zero"), (S Z, "un"), (S (S Z), "deux"), (S (S (S Z)), "trois")]

-- Il y a aussi une map vide et l'insertion explicite

-- >>> :t Map.empty
-- Map.empty :: Map k a

-- >>> :t Map.insert
-- Map.insert :: Ord k => k -> a -> Map k a -> Map k a

-- >>> Map.insert "e" 5 myMap
-- fromList [("a",1),("b",2),("c",3),("d",4),("e",5)]

-- >>> Map.insert (S (S (S (S Z)))) "quatre" natMap
-- fromList [(Z,"zero"),(S Z,"un"),(S (S Z),"deux"),(S (S (S Z)),"trois"),(S (S (S (S Z))),"quatre")]

-- On peut donc constuire une table de façon récursive, voici un exemple.

natNames :: Map Nat Text
natNames = build ["zero", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf"]
                 Z
  where build [] _ = Map.empty
        build (x:xs) n = Map.insert n x (build xs (S n))

-- >>> natNames
-- fromList [(Z,"zero"),(S Z,"un"),(S (S Z),"deux"),(S (S (S Z)),"trois"),(S (S (S (S Z))),"quatre"),(S (S (S (S (S Z)))),"cinq"),(S (S (S (S (S (S Z))))),"six"),(S (S (S (S (S (S (S Z)))))),"sept"),(S (S (S (S (S (S (S (S Z))))))),"huit"),(S (S (S (S (S (S (S (S (S Z)))))))),"neuf")]

{-

## Recherche

On peut accéder en O(log(n)) à la valeur associée à une clé, avec lookup

-}

-- >>> :t Map.lookup
-- Map.lookup :: Ord k => k -> Map k a -> Maybe a

-- >>> Map.lookup "b" myMap
-- Just 2

-- >>> Map.lookup "e" myMap
-- Nothing

-- >>> Map.lookup (S (S Z)) natMap
-- Just "deux"

-- >>> Map.lookup (S (S (S (S Z)))) natMap
-- Nothing

-- Il existe aussi un opérateur infixe (!?) similaire aux
-- séquences (et qu'on peut importer mais attention à ne pas confondre avec
-- par exemple le même opérateur dans les séquences)

-- Ici il a été importé explicitement pour les maps (cf. en haut du fichier)

-- >>> :t (!?)
-- (!?) :: Ord k => Map k a -> k -> Maybe a

-- >>> myMap !? "b"
-- Just 2

-- >>> myMap !? "e"
-- Nothing


{-

## Combinateurs pour les tables associatives

On retrouve nos fidèles map, filter et fold en différentes variantes

-}

{-
  ### Maps de maps
-}

-- On peut "mapper" les valeurs

-- >>> :t Map.map
-- Map.map :: (a -> b) -> Map k a -> Map k b

-- >>> Map.map (\x -> x * x) myMap
-- fromList [("a",1),("b",4),("c",9),("d",16)]

-- En fait, on peut très bien utiliser le fmap générique ...

-- >>> fmap (\x -> x * x) myMap
-- fromList [("a",1),("b",4),("c",9),("d",16)]

-- On peut aussi mapper les clés :

-- >>> :t Map.mapKeys
-- Map.mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a

-- >>> Map.mapKeys (\k -> k <> k) myMap
-- fromList [("aa",1),("bb",2),("cc",3),("dd",4)]

{-

 ### Filtrage

-}

-- Filtrage des valeurs

-- >>> :t Map.filter
-- Map.filter :: (a -> Bool) -> Map k a -> Map k a

-- >>> Map.filter even myMap
-- fromList [("b",2),("d",4)]

-- Filtrage des clés et valeurs

-- >>> :t Map.filterWithKey
-- Map.filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a

-- >>> Map.filterWithKey (\k v -> k /= "b") myMap
-- fromList [("a",1),("c",3),("d",4)]

{-

  ### Pliages (folds)

Il y a beaucoup de folds disponibles pour les tables associations.
Le principe est bien sûr de faire un parcours intégral, le plus souvent
 dans l'ordre des clés (puisque le type des clé est ordonné).

-}

-- il y a le foldr générique de Data.Foldable, qui travaille sur les valeurs.

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- pour un map, on a t = Map k   donc t a = Map k a   (pour un type de clé k)

-- >>> foldr (+) 0 myMapa
-- 10

-- >>> foldr (*) 1 myMap
-- 24

-- >>> foldr (:) [] myMap   
-- [1,2,3,4]   -- la liste des valeurs


-- Pour prendre en compte les clés, on utilise une opération spécifique
-- (et très utile en pratique)

-- >>> :t Map.foldrWithKey
-- Map.foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b

-- >>> Map.foldrWithKey (\k _ res -> k : res) [] myMap
-- ["a","b","c","d"]  -- la liste des clés

-- En guise d'exemple, retournons l'association de la valeur maximale 
-- (et du coup il faut pouvoir ordonner les valeurs)
keyOfMax :: Ord a => Map k a -> Maybe (k,a)
keyOfMax = Map.foldrWithKey step Nothing
  where step :: Ord a => k -> a -> Maybe (k,a) -> Maybe (k,a)
        step key val Nothing = Just (key, val)
        step key val (Just (maxKey, maxVal))
          | val > maxVal = Just (key, val)
          | otherwise = Just (maxKey, maxVal)   -- connaissez vous le @ dans les patterns ?

-- >>> keyOfMax myMap        
-- Just ("d",4)

-- >>> keyOfMax Map.empty
-- Nothing


{-

Il existe bien sûr un tas d'autres fonctions, à découvrir dans la documentation
complète du module.

-}
