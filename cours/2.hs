
module Cours2_Script where

-- # PAF 2 : Modélisation avec les types II
--           (a.k.a. Type-Driven Development)

-- ## - Types paramétrés
--      (polymorphisme paramétré)

-- ## - Types inductifs (aka. récursifs)

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

-- ==========================================================================================

-- # Les types paramétrés

-- Objectif : définition de types et fonctions génériques

-- ## Exemple classique : les valeurs optionnelles

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq)

-- Important: MyMaybe est appelée un constructeur de type, qui
-- a ici un paramètre: un type «quelconque»

-- Remarque : les paramètres de type sont des variables (de type)
-- et commencent en Haskell par des minuscules

-- >>> :t MyNothing
-- MyNothing :: MyMaybe a

-- .. qui signifie : MyNothing est de type (MyMaybe a), pour tout type a

-- >>> MyJust True
-- MyJust True

-- >>> :t MyJust True
-- MyJust True :: MyMaybe Bool

-- >>> :t MyJust (42 :: Integer)
-- MyJust (42 :: Integer) :: MyMaybe Integer

-- >>> :t MyJust 42
-- MyJust 42 :: Num a => MyMaybe a

-- >>> :t MyJust
-- MyJust :: a -> MyMaybe a

-- ==========================================================================================

-- # Maybe en Haskell

-- Le prélude définit le type Maybe a

-- >>> :info Maybe
-- type Maybe :: * -> *
-- data Maybe a = Nothing | Just a
--   	-- Defined in ‘GHC.Maybe’
-- instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (Maybe a)
--   -- Defined in ‘GHC.Base’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
-- instance Foldable Maybe -- Defined in ‘Data.Foldable’
-- instance Traversable Maybe -- Defined in ‘Data.Traversable’
-- instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance MonadFail Maybe -- Defined in ‘Control.Monad.Fail’
-- instance Monad Maybe -- Defined in ‘GHC.Base’

-- >>> :t Nothing
-- Nothing :: Maybe a

-- >>> :t Just
-- Just :: a -> Maybe a

-- >>> Just True
-- Just True

-- >>> Just (42 :: Integer)
-- Just 42

-- >>> Just 42
-- Just 42

-- ==========================================================================================

-- # Usages du Maybe

-- Le Maybe occupe une place centrale en programmation fonctionnelle
-- en général, et en Haskell en particulier
-- avec deux cas d'usage complémentaires :

-- ## 1) Modélisation de données/valeurs optionnelles

-- la notion d'«information optionnelle» est courante
-- Exemple : le digicode pour une adresse de livraison

-- ## 2) Fonctions «simplement» partielles

-- Certaines catégories de fonctions partielles peuvent être
-- «simulées» grâce au Maybe.

-- Exemple : la factorielle définie uniquement sur
-- les entiers naturels


-- ==========================================================================================

-- # 1) Modélisation de données/valeurs optionnelles

data AdresseLivraison = AdresseLivraison
     { numVoie :: Int
       , nomVoie :: String
       , digicode :: Maybe String
                   }
  deriving (Show)

-- >>> AdresseLivraison 1 "rue de la Paix" (Just "1407")
-- AdresseLivraison {numVoie = 1, nomVoie = "rue de la Paix", digicode = Just "1407"}

-- >>> AdresseLivraison 4 "place Jussieu" Nothing
-- AdresseLivraison {numVoie = 4, nomVoie = "place Jussieu", digicode = Nothing}

-- ==========================================================================================

-- # 2) Fonctions «simplement» partielles

fact :: Integer -> Maybe Integer
fact 0 = Just 1
fact n
  | n > 0 = case fact (n - 1) of
              Nothing -> Nothing
              Just k -> Just $ n * k   -- ou  Just (n * k)
  | otherwise = Nothing

-- >>> fact 4
-- Just 24


-- >>> fact (-1)
-- Nothing

factInfo :: Integer -> String
factInfo n = case fact n of
               Nothing -> "pas de valeur"
               Just k -> "la valeur est : " <> show k

-- >>> factInfo 4
-- "la valeur est : 24"

-- >>> factInfo (-2)
-- "pas de valeur"

factInfo' :: Integer -> String
factInfo' n = info (fact n)
  where info Nothing = "pas de valeur"
        info (Just k) = "la valeur est : " <> show k

-- >>> factInfo' 4
-- "la valeur est : 24"

-- >>> factInfo' (-2)
-- "pas de valeur"


-- ==========================================================================================

-- # Les types paramétrées et les kinds

-- Un type paramétré, comme (Maybe a), est un constructeur de type (ici à 1 argument)
-- Par exemple :  Maybe Int  est un type,    Maybe String  aussi, etc.

-- ### Question : quel est le «type» de   Maybe   «tout court» ?

-- ==========================================================================================

-- # Les types paramétrées et les kinds

-- Un type paramétré, comme (Maybe a), est un constructeur de type (ici à 1 argument)
-- Par exemple :  Maybe Int  est un type,    Maybe String  aussi, etc.

-- ### Question : quel est le «type» de   Maybe   «tout court» ?

-- ### Réponse : le «type» d'un constructeur de type est appelé son «kind»

-- >>> :kind Int
-- Int :: *

-- >>> :kind String
-- String :: *

-- >>> :kind Maybe
-- Maybe :: * -> *

-- >>> :k MyMaybe
-- MyMaybe :: * -> *

-- >>> :k (MyMaybe Int)
-- (MyMaybe Int) :: *

-- ==========================================================================================

-- # Règles (informelles) de kinding

-- 1) Les types non-paramétrés ont tous le kind *

-- >>> :k Integer
-- Integer :: *

-- >>> :k Bool
-- Bool :: *

-- >>> :k (Maybe Integer)
-- (Maybe Integer) :: *

-- 2) Les types paramétrés à n arguments ont le kind :

-- * -> * -> ... -> * -> *
-- \________________/
--     n fois

-- >>> :k Maybe
-- Maybe :: * -> *

-- Exemple de constructeur de type à 2 arguments :

-- >>> :k Either
-- Either :: * -> * -> *

-- ==========================================================================================

-- # Règles (formelles) de kinding

-- Syntaxe des kinds:

-- k ::=  *   |   * -> k

-- Règle de kinding

-- F :: * -> k     T :: *
-- ----------------------
--     (F T) :: k

-- ==========================================================================================

-- # Règles (formelles) de kinding

-- Syntaxe des kinds:

-- k ::=  *   |   * -> k

-- Règle de kinding

-- F :: * -> k     T :: *
-- ----------------------
--     (F T) :: k

-- ### Remarque: Cela ressemble à la règle de typage des applications :

-- f :: T -> U    e :: T
-- ---------------------
--    (f e) :: U

-- ==========================================================================================

-- # Un deuxième exemple : Either

data MyEither a b =
  MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- >>> MyLeft "Ceci est une erreur"
-- MyLeft "Ceci est une erreur"

-- >>> MyLeft "Ceci est une erreur" == MyRight 12
-- False

-- >>> :t MyLeft "Ceci est une erreur"
-- MyLeft "Ceci est une erreur" :: MyEither String b

-- >>> :t MyRight (42 :: Integer)
-- MyRight (42 :: Integer) :: MyEither a Integer

-- >>> :t Left "erreur"
-- Left "erreur" :: Either String b

-- >>> :t Right (42 :: Integer)
-- Right (42 :: Integer) :: Either a Integer


-- ==========================================================================================

-- # Kind de MyEither ?

-- data MyEither a b =
--   MyLeft a
--  | MyRight b
--  deriving (Show, Eq)

-- ## Question : quel est le kind de MyEither ?

-- >>> :k MyEither
-- MyEither :: * -> * -> *

-- >>> :k MyEither Integer
-- MyEither Integer :: * -> *

-- >>> :k MyEither Integer Bool
-- MyEither Integer Bool :: *

-- >>> :k Either
-- Either :: * -> * -> *

-- >>> :k Either Integer
-- Either Integer :: * -> *

-- >>> :k Either Integer Bool
-- Either Integer Bool :: *


-- ==========================================================================================

-- # Either en Haskell

-- >>> :info Either
-- type Either :: * -> * -> *
-- data Either a b = Left a | Right b
-- (etc ...)

-- ### Exemple d'utilisation du Either
-- ==> un «Maybe avec cas d'erreur» explicite

-- >>> quot 42 3
-- 14

-- >>> quot 3 0
-- *** Exception: divide by zero

safeQuot :: Integer -> Integer -> Either String Integer
safeQuot _ 0 = Left "division par zero"
safeQuot n m = Right (quot n m)

-- >>> safeQuot 42 3
-- Right 14

-- >>> safeQuot 3 0
-- Left "division par zero"

-- En pratique : définition d'un type somme pour les différentes
--               erreurs possibles

-- ==========================================================================================

-- # Les types inductifs

-- ## Objectif : coder des structures permettant des calculs récursifs

-- ## Exemple (théorique) : les entiers de Peano

-- Un entier de Peano est : soit zéro, soit le successeur d'un entier de Peano

data PNat =
  Z
  | S PNat
  deriving (Show)

-- Les premiers entiers > 0 :

one, two, three :: PNat
one = S Z
two = S one -- ou S S Z
three = S two

-- >>> three
-- S (S (S Z))

-- >>> two

-- ==========================================================================================

-- # Motivation (1) : récursion structurelle
-- (schéma de récursion qui suit la structure du type)

-- ## Objectif : exprimer des calculs récursifs

-- ### Exemple : égalité 

eqn :: PNat -> PNat -> Bool
eqn Z Z = True
eqn (S x) (S y) = x `eqn` y
eqn _ _ = False 

-- >>> eqn two two
-- True

-- >>> eqn three two
-- False

-- ==========================================================================================

-- # Remarque : égalité structurelle avec (==)

-- ==> on peut «promouvoir» cette égalité à l'opérateur `==`

instance Eq PNat where
  (==) = eqn

-- >>> two == two
-- True

-- >>> three == two
-- False

-- On aurait aussi pu dériver (==) par définition
-- (cf. cours sur les typeclasses)

-- ==========================================================================================

-- # Exercice : addition de Peano

-- Définir la fonction suivante :

--data PNat =
--  Z
--  | S PNat
--  deriving (Show)

addn :: PNat -> PNat -> PNat
addn Z n = n
addn (S m) n = S (addn m n)

-- >>> addn two three
-- S (S (S (S (S Z))))

-- ## Exercices complémentaires :
-- multiplication, prédécesseur, soustraction, puissance, division, etc.

-- ==========================================================================================

-- # Motivation (2) : preuve de programme

-- Dans ce cours, nous insistons également sur l'intérêt de
-- la programmation fonctionnelle pour la sûreté logicielle
-- et (donc) la possibilité de raisonner simplement sur le
-- fonctionnement des programmes.

-- ## Principe d'induction structurelle

-- Pour chaque type inductif (paramétré ou non), on peut expliciter
-- (au moins) un principe d'induction permettant de prouver
-- des propriétés sur les fonctions qui manipulent ces types.

-- ==========================================================================================

-- # Principe d'induction structurelle

-- ## Exemple des entiers de Peano

-- data PNat =
--   Z
--   | S PNat

-- ### Le principe d'induction («minimal») est le suivant:

-- ∀prop :: PNat -> Bool,
--    prop Z == True
--    ∧ (∀n :: PNat, prop n == True ==> prop (S n) == True)
--    ==> ∀n :: PNat, prop n == True

-- ==========================================================================================

-- # Exemple de raisonnement 

prop_addnZr :: PNat -> Bool
prop_addnZr n = addn n Z == n

-- ### Preuve:

-- ### (1) cas de base n=Z
-- prop_addnZr Z
-- = addn Z Z == Z   {- équation prop_addnZr.1 -}
-- = Z == Z          {- équation addn.1 }
-- = True            {- égalité réflexive -}

-- ### Remarque : ici Haskell peut nous aider

-- >>> prop_addnZr Z

-- (on parle de «preuve par évaluation»)

-- ==========================================================================================

-- # Exemple de raisonnement : cas récursif

-- prop_addnZr :: PNat -> Bool
-- prop_addnZr n = addn n Z == n

-- ### Preuve:

-- ### (1) cas de base n=Z
-- (...)

-- ### (2) cas récursif
-- ### On considère  n :: PNat
-- ### et on suppose : prop_addnZr n == True (hypothèse d'induction)
-- prop_addnZr n == True
-- = (addn n Z == n) == True  {- Hind -}

-- ## Montrons : prop_addnZr (S n) == True
-- prop_addnZr (S n)
-- = addn (S n) Z == (S n)  {- equ. prop_addnZr.1 -}
-- = S (addn n Z) == (S n)  {- equ. addn.2 }
-- = S n == S n             {- par hypothèse d'induction Hind -}
-- = True                   {- réflexivité de l'égalité -}

-- ==========================================================================================

-- # Types inductifs paramétrés

-- ## Exemple des listes

data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- >>> :t Cons True (Cons False (Cons True Nil))
-- Cons True (Cons False (Cons True Nil)) :: List Bool

-- >>> :t Cons 1 (Cons 2 (Cons 3 Nil))
-- Cons 1 (Cons 2 (Cons 3 Nil)) :: Num a => List a

-- >>> :t Cons (1 :: Integer) (Cons 2 (Cons 3 Nil))
-- Cons (1 :: Integer) (Cons 2 (Cons 3 Nil)) :: List Integer

-- ==========================================================================================

-- # Listes en Haskell

-- ### Le type prédéfini des listes est : [a]

-- >>> :info []

-- ### La liste vide est : []

-- >>> :t []
-- [] :: [a]

-- ### Le constructeur «cons» est la fonction (:)

-- >>> True : False : True : []
-- [True,False,True]

-- >>> :t True : False : True : []
-- True : False : True : [] :: [Bool]

-- Exceptionnellement, pour ce cours 2 nous continuons
-- à utiliser le type (List a)

-- ==========================================================================================

-- # Schéma récursif sur les listes

-- data List a =
--   Nil
--  | Cons a (List a)

-- ### Exemple : longueur d'une liste (en Paeano)

longueur :: List a -> PNat
longueur Nil = Z
longueur (Cons _ xs) = S (longueur xs) 

-- >>> longueur (Cons True (Cons False (Cons True Nil)))
-- S (S (S Z))

-- ### Exercice : longeur d'une liste en Haskell
-- (qui s'appelle length dans le prélude)

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- >>> myLength [True, False, True]
-- 3

-- >>> length [True, False, True]
-- 3

-- ==========================================================================================

-- # Les listes : principe inductif

-- data List a =
--   Nil
--  | Cons a (List a)

-- ### Pour raisonner sur les listes, le principe d'induction
-- ### «minimal» est le suivant :

-- ∀a :: *,
--   ∀prop :: List a -> Bool
--      prop Nil == True
--      ∧ (∀xs :: List a,  prop xs == True ==> ∀x :: a, prop (Cons x xs) == True)
--     ==> ∀xs :: List a, prop xs == True

-- ### et sur les listes prédéfinies :

-- ∀a :: *,
--   ∀prop :: [a] -> Bool
--      prop [] == True
--      ∧ (∀x :: a, ∀xs :: [a],  prop xs == True ==> prop (x:xs) == True)
--     ==> ∀xs :: [a], prop xs == True

-- ==========================================================================================

-- # Exercice : raisonnement sur map

-- ### Soit la fonction suivante :

myMap :: (a -> b) -> List a -> List b
myMap f Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- >>> myMap (\x -> x + 1) (Cons 0 (Cons 1 (Cons 2 Nil)))
-- Cons 1 (Cons 2 (Cons 3 Nil))

-- ... qui s'appelle fmap sur les listes prédéfinies (entre autre!)

-- >>> fmap (\x -> x + 1)  [0, 1, 2]
-- [1,2,3]

-- >>> fmap (+1) [0, 1, 2]
-- [1,2,3]

-- ### et la fonction suivante :

comp :: (b -> c) -> (a -> b) -> a -> c
comp g f = \x -> g (f x)

-- ### ... qui s'appelle en fait .  (point)

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- >>> ((+1) . (*2)) 5
-- 11

-- ## Exercice : prouver la propriété ci-dessous

prop_mapcomp :: Eq c => (b -> c) -> (a -> b) -> List a -> Bool
prop_mapcomp g f xs =
  myMap (comp g f) xs == myMap g (myMap f xs)

-- (ou l'on suppose pouvoir tester l'égalité sur les valeurs du type c
--  , cf. cours sur les typeclasses)

