
module Set where

-- on va utiliser des chaînes de type Text
import Data.Text (Text)
import qualified Data.Text as Text

{-

# Les ensembles (sets)

Les ensembles sont très utiles en modélisation, et s'apparentent
aux ensembles mathématiques : des collections d'objets.

Un ensemble de type `Set a` est une collection finie et non-ordonnée d'éléments
 d'un même type a et sans doublon.

Pour utiliser les ensembles de la bibliothèque containers,
on doit importer le module Data.Set.

Remarque : on pourrait utiliser Data.Map pour remplacer Data.Set
en considérant des tables associatives avec le type (Map a ())
(avec le type `()` le type qui ne contient qu'une valeur `()`).
Cependant, les opérations du module Data.Set sont plus explicitement
dédiées aux ensembles, et sont donc privilégiées.

-}

-- on importe le type Set et quelques opérations ensemblistes
-- que l'on ne veut pas préfixer
import Data.Set (Set, isSubsetOf, union, intersection, difference) 
import qualified Data.Set as Set

{-

## Construction d'un ensemble

-}

-- L'ensemble élémentaire est l'ensemble vide :

-- >>> :t Set.empty
-- Set.empty :: Set a

-- Un singleton est un ensemble ne contenant qu'un élément :

-- >>> :t Set.singleton
-- Set.singleton :: a -> Set a

-- >>> Set.singleton 42
-- fromList [42]

-- On peut bien sûr utiliser fromList pour construire un ensemble à partir
-- d'une liste :

mySet :: Set Text
mySet = Set.fromList ["a", "b", "c", "d", "e"]

-- Il est aussi possible d'ajouter une élément à un ensemble avec : insert

-- >>> :t Set.insert
-- Set.insert :: Ord a => a -> Set a -> Set a

-- >>> Set.insert "f" mySet
-- fromList ["a","b","c","d","e","f"]

-- et bien sûr les doublons ne sont pas ajoutés

-- >>> Set.insert "b" mySet
-- fromList ["a","b","c","d","e"]

-- On remarque que le type a d'un ensemble de type `Set a` doit être ordonné
-- (i.e. satisfaire la contrainte de la typeclass Ord)
-- On peut reprendre l'exemple des entiers naturels pour illustrer
-- ce point.

data Nat =
  Z
  | S Nat
  deriving (Show, Eq, Ord)

-- >>> :t Set.fromList [Z, S Z, S (S Z), S (S (S Z)), S (S (S (S Z)))]
-- Set.fromList [Z, S Z, S (S Z), S (S (S Z)), S (S (S (S Z)))] :: Set Nat

-- On illustre ci-dessous la construction récursive d'un ensemble

nats :: Integer -> Set Nat
nats n | n <= 0 = Set.empty
       | otherwise = build 0 Z
  where build m p | m == n = Set.singleton p
                  | otherwise = Set.insert p (build (m + 1) (S p))

-- >>> nats 5
-- fromList [Z,S Z,S (S Z),S (S (S Z)),S (S (S (S Z))),S (S (S (S (S Z))))]

-- La taille (le cardinal) d'un ensemble est le nomre d'éléments qu'il contient :

-- >>> :t Set.size
-- Set.size :: Set a -> Int

-- >>> Set.size mySet
-- 5

-- >>> Set.size (nats 10)
-- 11

-- Et on peut tester si un ensemble est vide avec `null`

-- >>> :t Set.null
-- Set.null :: Set a -> Bool

-- >>> Set.null Set.empty
-- True

-- >>> Set.null mySet
-- False

{-

## Test d'appartenance

L'opération principale d'un ensemble est le test d'appartenance `member`,
 qui est O(log(n)) au pire cas (d'après la documentation).

-}

-- >>> Set.member "b" mySet
-- True

-- >>> Set.member "f" mySet
-- False

-- >>> Set.member (S (S Z)) (nats 3)
-- True

{-

## Sous-ensembles et égalité ensemblistes

On peut vérifier si un ensemble est sous-ensemble d'un autre
ensemble avec `isSubsetOf`

-}

-- >>> Set.isSubsetOf (Set.fromList ["a", "b"]) (Set.fromList ["a", "b", "c"])
-- True

-- >>> Set.isSubsetOf (Set.fromList ["a", "b", "c"]) (Set.fromList ["a", "b"])
-- False

-- On préfère en général utiliser cette fonction en position opérateur
-- (et en faisant un import explicite)

-- >>> Set.fromList ["a", "b", "c"]  `isSubsetOf` Set.fromList ["a", "b", "c"]
-- True

-- Attention, les opérateurs de comparaisons <= et >= sont disponibles mais
-- appliquent un ordre lexicographique (qui ne semble pas très utile ...)

-- >>> Set.fromList ["a", "b"] <= Set.fromList ["c"]
-- True   -- ???

-- En revanche, l'égalité ensembliste est bien disponible sous la forme
-- de l'opérateur == :

-- >>> Set.fromList ["a", "b", "c"] == Set.fromList ["c", "a", "b", "a", "c"]
-- True


{-

### Opérations ensemblistes

Les opérateurs ensemblistes de base sont l'union, la différence et l'intersection.
On les utilise en général en position infixe (en les important explicitement) 

-}

-- >>> mySet `union` mySet
-- fromList ["a","b","c","d","e"]

-- >>> mySet `union` Set.empty
-- fromList ["a","b","c","d","e"]

-- >>> (Set.fromList ["a", "b"]) `union` (Set.fromList ["b", "c", "d"])
-- fromList ["a","b","c","d"]

-- >>> mySet `intersection` mySet
-- fromList ["a","b","c","d","e"]

-- >>> mySet `intersection` Set.empty
-- fromList []

-- >>> (Set.fromList ["a", "b"]) `intersection` (Set.fromList ["b", "c", "d"])
-- fromList ["b"]

-- >>>  mySet `difference` mySet
-- fromList []

-- >>> mySet `difference` Set.empty
-- fromList ["a","b","c","d","e"]

-- >>> (Set.fromList ["a", "b"]) `difference` (Set.fromList ["b", "c", "d"])
-- fromList ["a"]

-- >>> (Set.fromList ["b", "c", "d"]) `difference` (Set.fromList ["a", "b"])
-- fromList ["c","d"]

{-

### Combinateurs d'ensembles

On dispose bien sûr des combinateurs habituels: map, filter et fold

-}

-- pour le map, on ne peut pas utiliser le fmap générique, ce qui est dû
-- à la contrainte d'ordre (Ord) imposée sur les éléments d'un ensemble.
-- il y a donc une fonction spécifique.

-- >>> :t Set.map
-- Set.map :: Ord b => (a -> b) -> Set a -> Set b

-- >>> Set.map (\x -> x <> x <> x) mySet
-- fromList ["aaa","bbb","ccc","ddd","eee"]

-- On peut bien sûr filtrer un ensemble pour un prédicat

-- >>> :t Set.filter
-- Set.filter :: (a -> Bool) -> Set a -> Set a

-- >>> Set.filter even (Set.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9])
-- fromList [2,4,6,8]

-- Et il existe plusieurs fold, on en prend un "arbitrairement" :

-- >>> :t Set.foldr
-- Set.foldr :: (a -> b -> b) -> b -> Set a -> b


-- >>> Set.foldr (<>) "" mySet
-- "abcde"

-- >>> Set.fold (+) 0 (Set.fromList [1, 2, 3, 4, 5])
-- 15

-- >>> Set.fold (*) 1 (Set.fromList [1, 2, 3, 4, 5])
-- 120


-- En guise d'exemple, on cherche le maximum dans un ensemble

-- choix d'un élément "arbitraire"
choose :: Set a -> a
choose = Set.elemAt 0

searchMax :: Ord a => Set a -> Maybe a
searchMax ens = if Set.null ens
                then Nothing
                else Just $ Set.foldr max (choose ens) ens

-- >>> searchMax mySet
-- Just "e"

-- >>> searchMax Set.empty
-- Nothing

-- >>> searchMax (Set.fromList [2, 4, 5, 3, 42, 9, 33])
-- Just 42

-- >>> searchMax (nats 6)
-- Just (S (S (S (S (S (S Z))))))

-- Remarque : on a ajouté la possibilité de "choisir" un élément
-- dans l'ensemble... en choisissant le "premier" élément avec `elemAt`.
-- La représentation permet en effet d'indexer les éléments, dans
-- l'ordre imposé par la contrainte sur le type a...
-- On consultera la documentation pour ces fonctionnalités un peu
-- plus "avancées".

