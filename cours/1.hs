
module Cours1_Script where

-- # PAF 1 : Modélisation avec les types I
--           (a.k.a. Type-Driven Development)

-- ## - Types de base
--      (booléens, nombres, etc.)

-- ## - Types sommes-de-produits et records

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

-- =====================================================

-- # 1) les booléens : le type Bool

-- le vrai s'appelle:  True

-- >>> :t True
-- True :: Bool

-- le faux s'appelle:  False

-- >>> :t False
-- False :: Bool

-- ## Exemple de fonction qui manipule des booléens :

-- >>> :t not
-- not :: Bool -> Bool

-- >>> not True
-- False

-- >>>  not False
-- True

-- >>> not (not True)
-- True

-- =====================================================

-- # Une première fonction

-- Exemple : on peut réécrire la négation sous forme d'une fonction utilisateur

non :: Bool -> Bool   -- signature (type de la fonction)
non True = False      -- première équation  (non.1)
non False = True      -- seconde équation  (non.2)

-- >>> non True
-- False

-- >>> non False
-- True

-- >>> non $ non True
-- True

-- ### A remarquer : le $ <= opérateur d'application
--   f $ a b        est équivalent à   f (a b) 
--   f $ a b c      est équivalent à   f (a b c)
--   f $ a b $ c d  est équivalent à   f (a b (c d))
--   etc ...
-- 

-- =====================================================

-- # Remarque : fonction vs. opérateur

-- Voici une définition Haskell du et-logique

et :: Bool -> Bool -> Bool
et True True = True
et _ _ = False

-- >>> et True (not False)
-- True

-- >>> et True (not (not False))
-- False

-- ## Important : En Haskell l'évaluation est "paresseuse" par défaut
-- (on y reviendra ...)

-- >>> et True (error "boum")
-- *** Exception: boum
-- CallStack (from HasCallStack):
--   error, called at <interactive>:73:11 in interactive:Ghci1

-- >>> et False (error "boum")
-- False

-- ### Question : une telle définition est-elle possible dans un langage
-- ### de programmation «classique» ?


-- =====================================================

-- ## Remarque 1 :

-- Dans le prélude, le et logique se nomme `&&`:

-- >>> True && (not False)
-- True

-- >>> False && error "boum"
-- False

-- =====================================================

-- ## Remarque 2 :

--      Notation infixe pour les "fonctions binaires"

-- >>> True `et` (not False)
-- True

--      vs. opérateur en position fonction

-- >>> (&&) True (not False)
-- True

-- =====================================================

-- ## Exercice :  définir  le ou logique (||)

ou :: Bool -> Bool -> Bool
ou _ _ = undefined

-- >>> ou False False

-- >>> ou False True

-- >>> ou True False

-- >>> ou True True

-- =====================================================

-- Dans le prélude, le ou logique s'appelle `||` :

-- >>> False || False
-- False

-- >>> False || True
-- True

-- >>> True || False
-- True

-- >>> True || True
-- True

-- >>> True || error "boum"
-- True

-- =====================================================

-- ## L'implication logique
-- (qui se révélera bien utile !)

-- Introduisons une notation infixe :
(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True

infixr 1 ==>

-- opérateur infixe associatif à droite (right)
-- et de priorité 1    (sur 9)

-- >>> (2 > 3) ==> (2 > 4)
-- True

-- >>> (2 < 3) ==> (2 > 4)
-- False

-- =====================================================

-- # Propriétés (prédicats)

-- Une propriété, (ou prédicat), est une fonction dont le
-- type de retour est booléen.

-- Par convention : on utilisera le préfixe `prop` pour les propriétés.

prop_andCommutes :: Bool -> Bool -> Bool
prop_andCommutes a b = a && b ==> b && a

-- >>> prop_andCommutes True False
-- True

-- =====================================================
  
-- # Les alternatives

-- ## Observation :
-- Le if-then-else peut s'écrire comme une fonction
  
si :: Bool -> a -> a -> a
si True ethen _ = ethen
si False _ eelse = eelse

-- >>> si (4 > 3) 42 12
-- 42

-- >>> si (4 < 3) 42 12
-- 12

-- ## Le if-then-else de Haskell est un simple «sucre syntaxique» :

-- >>>  if  4 > 3  then  42 else 12
-- 42

-- >>>  if  4 < 3  then  42 else 12
-- 12

-- =====================================================

-- # 2) les entiers

-- On dispose :

-- - soit du type Integer pour les grands entiers

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- >>> fact 5
-- 120

-- >>> fact 30
-- 265252859812191058636308480000000

-- - soit du type Int pour les entiers machine (64bits, en général)

factInt :: Int -> Int
factInt 0 = 1
factInt n = n * factInt (n - 1)

-- >>> factInt 5
-- 120

-- >>> factInt 30
-- -8764578968847253504

-- ## En pratique, on utilisera le plus souvent Integer, plus «sûr»

-- =====================================================

-- ## A propos de la récursion terminale

-- ==> en Haskell on est moins "omnubilé" par la récursion terminale
--     qu'en Scheme ou Ocaml par exemple.
--     (en raison de l'évaluation paresseuse, notamment)

-- Cependant, Haskell (ghc) élimine "la plupart" des appels terminaux

factit :: Integer -> Integer -> Integer
factit 0 acc = acc
factit n acc = factit (n - 1) (n * acc)

-- >>> factit 10 1
-- 3628800

-- =====================================================

-- ## Utilisation de définitions auxiliaires

-- ### - avec `where`

factit2 :: Integer -> Integer
factit2 n = aux n 1
  where aux 0 acc = acc
        aux n acc = aux (n - 1) (n * acc)

-- >>> factit2 10
-- 3628800

-- ### - ou avec `let`

-- -- <<<WARNING>>> : style "ocaml-ien" déconseillé en Haskell !
factit3 :: Integer -> Integer
factit3 n = 
  let aux k acc =
        case k of
          0 -> acc
          m -> aux (m - 1) (m * acc)
  in aux n 1

-- >>> factit3 10
-- 3628800

-- Remarque : on privilégie `where` pour les définitions locales de fonctions
--            et `let ... in ...` pour les définitions locales de variables

-- =====================================================

-- ## Les combinateurs

-- En Haskell on va (très) rapidement privilégier des schémas de récursion avec
-- combinateur comme foldr (paresseux), foldl' (strict), foldMap, etc.

factit4 :: Integer -> Integer
factit4 n = foldr (*) 1 [1..n]

-- >>> factit4 10
-- 3628800

-- =====================================================


-- # Autres types numériques

-- Haskell propose bien sûr la plupart des types numériques utiles

-- ## Le type Float : flottants simple précision

-- >>> (4.2 :: Float) * 9
-- 37.8

-- ## Le type Double : flottants double précision

-- >>> (4.2 :: Double) * 9
-- 37.800000000000004

-- Remarque : les flottants, c'est un peu comme les  ______________________________
--            .... tant qu'on peut s'en passer, il faut les éviter !


-- ## ... et les rationnels, les complexes, etc.


-- =====================================================


-- # 3) Types sommes-de-produits

-- Les types sommes correspondent (comme vous le savez) à
-- des unions disjointes.

-- Exemple : les booléens "maison"

data MyBool =
  BTrue     -- vrai
  | BFalse  -- faux

-- Il y a deux valeurs possibles du type `MyBool`.

-- >>> :t BTrue
-- BTrue :: MyBool

-- >>> :t BFalse
-- BFalse :: MyBool

-- Ce sont les deux *constructeurs de données* pour le type
-- (==> data constructors)

-- =====================================================

-- # Data-constructeurs

-- Chaque (data-)constructeur correspond à un produit

-- ## Exemple :

data MoyenPaiement =
  Cheque Int  -- numéro du chèque
  | Carte String String -- nom du porteur ⨯ numéro de CB
  | Espece

-- ### Attention :
--     En Haskell le nom du type et le nom des constructeurs
--     doivent commencer par une majuscule.

-- >>> :t Cheque 42
-- Cheque 42 :: MoyenPaiement

-- >>> :t Carte "Toto" "1423 4242 3242 2124"
-- Carte "Toto" "1423 4242 3242 2124" :: MoyenPaiement

-- >>> :t Espece
-- Espece :: MoyenPaiement

-- =====================================================

-- # Pattern matching

-- Le pattern matching permet de "déconstruire" les données

-- ### Exemple de fonction :
--       Description d'un moyen de paiement

description :: MoyenPaiement -> String
description mp = "Paiement par " <> (describe mp)
  where describe (Cheque n) = "Cheque No" <> (show n)
        describe (Carte nom num) = "Carte No" <> num <> ", nom : " <> nom
        describe Espece = "Espece"

-- >>> description (Cheque 424242)
-- "Paiement par Cheque No424242"

-- >>> description (Carte "Toto" "1423 4242 3242 2124")
-- "Paiement par Carte No1423 4242 3242 2124, nom : Toto"

-- >>> description Espece
-- "Paiement par Espece"

-- =====================================================

-- ### Remarque :
--   Il est également possible de faire du pattern-matching
-- explicite, mais c'est moins recommandé en Haskell
-- (on privilégie le style et le raisonnement équationnel)

description2 :: MoyenPaiement -> String
description2 mp = "Paiement par " <>
  case mp of
    (Cheque n) -> "Cheque No" <> (show n)
    (Carte nom num) -> "Carte No" <> num <> ", nom : " <> nom
    Espece -> "Espece"

-- =====================================================

-- ## Les produits cartésiens

-- En dehors des sommes-de-produits, 
-- Haskell permet bien sûr de manipuler des produits
-- "classiques" (n-uplets)

-- ### Exemples :

-- >>> :t (2 :: Integer, "toto")
-- (2 :: Integer, "toto") :: (Integer, String)

-- >>> fst (2 :: Integer, "toto")
-- 2

-- >>> snd (2 :: Integer, "toto")
-- "toto"

-- >>> :t (2 :: Integer, "toto", True)
-- (2 :: Integer, "toto", True) :: (Integer, String, Bool)


inv3 :: (a, b, c) -> (c, b, a)
inv3 (x, y, z) = (z, y, x)

-- >>> inv3 (2 :: Integer, "toto", True) 
-- (True,"toto",2)


-- =====================================================

-- # 4) Les records

-- ### Motivations ...

-- Voici un autre exemple de produit :

type Name = String -- alias de type
type Age = Int

data Person = Person Name Age
  deriving Show   -- on verra cette annotation en TME

-- >>> :t Person "yoda" 700
-- Person "yoda" 700 :: Person

-- >>> Person "yoda" 700
-- Person "yoda" 700

-- ## Remarques :

-- - `type` introduit un alias de type
--    alors que `data` introduit un nouveau type.

-- - le (data-)constructeur `Person` est identique au nom du type,
--   c'est une convention pour les "produits purs"

-- - `Person` est un type isomorphe mais non identique aux couples
--   de type `(String, Int)` 

-- =====================================================

-- # Accesseurs et mises à jour

-- ## Accesseurs
personName :: Person -> Name
personName (Person name _) = name

personAge :: Person -> Age
personAge (Person _ a) = a

-- >>> personName (Person "yoda" 700)
-- "yoda"

-- >>> personAge (Person "yoda" 700)
-- 700

-- ## Mise à jour
anniversary :: Person -> Person
anniversary (Person name age) = Person name (age + 1)

-- >>> anniversary (Person "yoda" 700)
-- Person "yoda" 701

-- ### ... cela fait un peu trop de "boiler plate", non ?

-- =====================================================

-- # Les records en Haskell

-- Les types records (enregistrements) sont des sortes de "produits nommés".

-- Voici un exemple :

data PersonInfo = PersonInfo {
  firstName, middleName, lastName :: Name
  , age :: Age
  , login :: String
  , birthYear :: Int } 
  deriving Show

jean :: PersonInfo
jean = PersonInfo "jean" "jean" "jr" 42 "jean" 1956

-- >>> :t jean
-- jean :: PersonInfo

-- =====================================================

-- ## Accesseurs auto-générés

-- >>> :t firstName
-- firstName :: PersonInfo -> Name

-- >>> firstName jean
-- "jean"

-- >>> birthYear jean
-- 1956

-- =====================================================

-- # Expression de mise-à-jour (update)

setLogin :: PersonInfo -> String -> PersonInfo
setLogin pers logg = pers { login = logg }

-- >>> setLogin jean "michel"
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 42, login = "michel", birthYear = 1956}

-- =====================================================

-- # Record = produit

-- A part cela, les records se comportent exactement comme
-- des produits "normaux"

setLoginMoche :: PersonInfo -> String -> PersonInfo
setLoginMoche (PersonInfo f m l a _ b) logg = PersonInfo f m l a logg b

anniversaryBof :: PersonInfo -> PersonInfo
anniversaryBof (PersonInfo f m l a lg b) =
  PersonInfo f m l (a + 1) lg b

-- Avec l'expression d'update, c'est un peu mieux
anniversaryPotable :: PersonInfo -> PersonInfo
anniversaryPotable pers@(PersonInfo _ _ _ a _ _) = pers { age = a + 1 }

-- >>> jean
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 42, login = "jean", birthYear = 1956}

-- >>> anniversaryPotable jean
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 43, login = "jean", birthYear = 1956}

-- =====================================================

-- # Pattern matching de records

-- Syntaxe de déconstruction dédiée

nameAge :: PersonInfo -> (Name, Age)
nameAge PersonInfo { lastName = n, age = a} = (n, a)

-- >>> nameAge jean
-- ("jr",42)

-- La version la plus concise
anniversaryBest :: PersonInfo -> PersonInfo
anniversaryBest pers@(PersonInfo { age = a}) = pers { age = a + 1}

-- >>> anniversaryBest jean
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 43, login = "jean", birthYear = 1956}

-- =====================================================

-- # A retenir

-- les types records ne sont pas primitifs en Haskell,
-- il s'agit simplement de sucre syntaxique pour des types produits "normaux".
-- (avec des avantages et des inconvénients)
