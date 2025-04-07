
module Seq where

-- on va utiliser des chaînes de type Text
import Data.Text (Text)

{-

# Les séquences

Les séquence de type : Seq a
sont une variante de listes avec des possibilités
supplémentaires et des performances en général
meilleures que les listes de type [a]

Cependant, on utilisera également les listes [a]
dans plusieurs situations :
- pour permettre de construire des conteneurs avec fromList
- ... et donc pour échanger des données
- pour modéliser une "pile fonctionnelle", possible aussi avec
  les séquences mais sans gain apparent
- pour manipuler des "listes paresseuses" (parfois appelées streams ou flots ...)

-}

-- Pour commencer, il faut importer le module des séquences
import Data.Sequence (Seq (..), (!?))
-- on importe le type Seq et ses constructeurs (patterns)
-- ainsi que certaines fonctions que l'on ne veut pas préfixer.

import qualified Data.Sequence as Seq
-- on préfixe les fonctions avec Seq

-- Pour pouvoir faire des folds de façon confortable, il faut importer
-- le module suivant

import Data.Foldable  (null, foldr)
-- on importe ici explicitement les fonctions que l'on va utiliser
-- mais il est courant d'importer ce module en entier, comme
-- une sorte d'extension du prélude
                            

{-

## Construction de séquences

On utilise souvent fromList pour construire une séquence explicite

-}

-- >>> :t Seq.fromList
-- Seq.fromList :: [a] -> Seq a

intSeq :: Seq Integer
intSeq = Seq.fromList [1, 2, 3, 4, 5]

txtSeq :: Seq Text
txtSeq = Seq.fromList ["a", "b", "c", "d", "e"]

-- Il y a aussi une liste vide que l'on peut construire avec :

-- 1) soit la fonction empty

-- >>> :t Seq.empty
-- Seq.empty :: Seq a

-- >>> Seq.empty
-- fromList []

-- 2) soit directement le constructeur Empty (si on a importé les constructeurs)

-- >>> Empty
-- fromList []

-- On peut tester si une liste est vide ou non, soit par pattern matching
-- soit avec la fonction null

-- >>> :t Seq.null
-- Seq.null :: Seq a -> Bool

-- >>> Seq.null intSeq
-- False

-- >>> Seq.null Empty
-- True

-- (en fait, on a aussi null dans Data.Foldable ...)

-- >>> :t null
-- null :: Foldable t => t a -> Bool

-- >>> null Empty
-- True

-- On peut également construire par ajout au début, comme les listes

-- >>> 0 :<| intSeq
-- fromList [0,1,2,3,4,5]

-- Ou en fin :

-- >>> intSeq :|> 6
-- fromList [1,2,3,4,5,6]

-- Point important : ces deux opérations sont très efficaces, en O(1)

-- On peut récupérer la longueur de la séquence (efficacement) :

-- >>> :t Seq.length
-- Seq.length :: Seq a -> Int

-- >>> Seq.length intSeq
-- 5

-- Il est donc possible de construire récursivement une séquence :

interval :: Integer -> Integer -> Seq Integer
interval m n | m > n = Empty
             | otherwise = m :<| interval (m + 1) n

-- >>> interval 2 6
-- fromList [2,3,4,5,6]

-- Et si on construit à partir de la fin ...
lavretni :: Integer -> Integer -> Seq Integer
lavretni m n | m > n = Empty
             | otherwise = lavretni (m + 1) n :|> m

-- >>> lavretni 2 6             
-- fromList [6,5,4,3,2]

-- Petite remarque, on peut donc faire un map récursif terminal
-- sans renversement donc en un seul passage dans la séquence ... quid des listes ?

myMap :: (a -> b) -> Seq a -> Seq b
myMap f xs = aux xs Empty
  where aux Empty res = res
        aux (x :<| xs) res = aux xs (res :|> (f x))

-- >>> myMap (\x -> x * x) intSeq
-- fromList [1,4,9,16,25]

{-

## Accès aux éléments par leur indice

Contrairement aux listes "classiques", on peut accéder aux éléments
par leur indice dans la séquence (en commençant par 0).

-}

-- >>> :t Seq.index
-- Seq.index :: Seq a -> Int -> a

-- >>> Seq.index txtSeq 0
-- "a"

-- >>> Seq.index txtSeq 3
-- "d"

-- Attention : il s'agit d'une fonction partielle

-- >>> Seq.index txtSeq 6
-- "*** Exception: index out of bounds in call to: Data.Sequence.index 6
-- ... blabla ...

-- Il existe une version plus sûre, et donc recommandée, l'opérateur !?

-- >>> :t (Seq.!?)
-- (Seq.!?) :: Seq a -> Int -> Maybe a

-- On va l'importer explicitement, cf. la déclaration au début du fichier

-- >>> txtSeq !? 0
-- Just "a"

-- >>> txtSeq !? 3
-- Just "d"

-- >>> txtSeq !? 6
-- Nothing

{-

## Les combinateurs de séquences

Le style fonctionnel privilégie les combinateurs plutôt que la récursion
explicite (pour la sûreté, l'efficacité, la concision, etc.)

On va bien sûr pouvoir faire du map, du filter et tout un tas de fold...

Important : pour les folds il faut impérativement importer Data.Foldable

-}

-- On a déjà écrit un map, mais le fmap générique de Haskell s'applique aux séquences

-- >>> fmap (\x -> x * x) (interval 1 11)
-- fromList [1,4,9,16,25,36,49,64,81,100]

-- il y a un combinateur filter

-- >>> :t Seq.filter
-- Seq.filter :: (a -> Bool) -> Seq a -> Seq a

-- Exemple : les entiers pairs dans l'intervalle [1;10]

-- >>> Seq.filter even (interval 1 11)
-- fromList [2,4,6,8,10]

-- pour fold, on a un foldr générique qui vient de Data.Foldable

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

{-
On doit passer :
   1) une fonction à deux arguments
         l'élément courant de type a
         le résultat accumulé de type b
      et qui doit retourner le résultat accumulé en
      prenant en compte l'élément courant, donc de type b
   2) une "résultat accumulé" initial, de type b
   3) une structure "pliable" (Foldable) comme une séquence notamment,
      et dans ce cas on a t=Seq   dont t a = Seq a

Et le foldr retourne le résultat accumulé pour l'ensemble du parcours
de la séquence, de type b.

On prend un exemple classique : la somme

-}

-- En version très explicite :
somme :: Seq Integer -> Integer
somme xs = foldr (\elem res -> elem + res) 0 xs

-- >>> somme (interval 1 11)
-- 55

-- En style "Haskell-fan" ...
somme' :: Seq Integer -> Integer
somme' = foldr (+) 0

-- (on peut rester sur la première version tant qu'on ne comprend pas que c'est la même fonction)

-- Exercice : le produit

-- Il existe plusieurs types de fold : foldl, foldl', foldM, etc. cf. Data.Foldable
-- ... Les distinguer précisément demande un peu d'expérience...
-- on va rester sur foldr pour l'instant...

-- Il existe aussi un fold spécifique aux séquences, qui permet de parcourir
-- la séquence des éléments avec leur indice.

-- >>> :t Seq.foldrWithIndex
-- Seq.foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b

-- Ici la fonction de fold prend un argument supplémenter : l'indice de l'élément courant
-- (un Int donc entier machine)

-- On peut par exemple récupérer l'indice de l'élément maximal dans une séquence,
-- avec sa valeur (pour être précis, le premier max)

maxAndIndex :: Seq Integer -> Maybe (Integer, Int)
maxAndIndex Empty = Nothing
maxAndIndex (x :<| xs) = Just $ Seq.foldrWithIndex step (x, 0) xs
  where step :: Int -> Integer -> (Integer, Int) -> (Integer, Int)
        step index elemCourant (max, indexMax)
          | elemCourant > max = (elemCourant, index)
          | otherwise = (max, indexMax)

-- Les cas d'utilisation de ce type de fold sont probablement assez rares ...


-- Il y a bien sûr beaucoup d'autres fonctions permettant de manipuler
-- les séquences,  mais on dispose de de quoi démarrer et il suffira
-- maintenant d'aller lire la documentation  (cf dans le README.md)
