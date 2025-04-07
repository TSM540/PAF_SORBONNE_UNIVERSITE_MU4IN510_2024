-- Pour typer explicitement les méthodes
{-# LANGUAGE InstanceSigs #-}

module Cours7_Script where

import Data.List (foldl')

-- Module associé aux foncteurs applicatifs
import Control.Applicative

--------------------------------------------------------------------------
-- # PAF 7 : Structures algébriques (2/3)

-- #         Foncteurs Applicatifs

-- ## - Rappels sur les Foncteurs (typeclass Functor)

-- ## - Foncteurs applicatifs (typeclass Applicative)

-- ## - Application : les structures «traversables»
-- ## (typeclass Traversable)

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

--------------------------------------------------------------------------
-- # Rappels : les Foncteurs

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a

-- ## Interprétation de (fmap g) :

-- Appliquer une fonction unaire g
-- dans un contexte fonctoriel f :: * -> *

--------------------------------------------------------------------------
-- # Exemple 1 : contexte des paires (conteneur)

plusUn :: Int -> Int
plusUn n = n + 1

-- ## fmap applique la fonction unaire plusUn ...

-- >>> fmap plusUn (True, 41)
-- (True,42)

-- ## ... dans le contexte   (,) Bool

-- >>> :kind (,)
-- (,) :: * -> * -> *

-- >>> :kind (,) Bool
-- (,) Bool :: * -> *

-- ### ici :

-- fmap :: (a -> b) -> (Bool, a) -> (Bool, b)

--------------------------------------------------------------------------
-- # Exemple 2 : contexte des calculs optionnels

-- >>> :t plusUn
-- plusUn :: Int -> Int

-- ## fmap applique la fonction unaire plusUn ...

-- >>> fmap plusUn Nothing
-- Nothing

-- >>> fmap plusUn (Just 41)
-- Just 42

-- ## ... dans le contexte  Maybe

-- >>> :kind Maybe
-- Maybe :: * -> *

-- ### ici :

-- fmap :: (a -> b) -> Maybe a -> Maybe b

--------------------------------------------------------------------------
-- # Foncteur = application contextuelle unaire

-- ## Deux interprétations équivalentes ...

-- ## (1) «Lifter» une fonction unaire dans un contexte
-- ### si
-- g :: a -> b
-- ### alors
-- fmap g :: f a -> f b

-- ## (2) Appliquer g à une valeur dans le contexte
-- ### si
-- g :: a -> b
-- x :: f a
-- ### alors
-- fmap g x :: f b

-- ### Pour cette interprétation, on utilisera souvent
-- ### la version «opérateur» du fmap : <$>

-- >>> :info (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
--   	-- Defined in ‘Data.Functor’
-- infixl 4 <$>

-- ## On a : g <$> x ≡ g `fmap` x ≡ fmap g x

-- >>> plusUn <$> (True, 41)
-- (True,42)

-- >>> plusUn <$> Nothing
-- Nothing

-- >>> plusUn <$> (Just 41)
-- Just 42

-- ### Il s'agit de la variante «contextuelle» de $

-- >>> :t ($)
-- ($) :: (a -> b) -> a -> b

-- >>> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

--------------------------------------------------------------------------
-- # Fonctions d'arité > 1 ?

-- ### Prenons les fonctions suivantes :

plus :: Int -> Int -> Int
plus m n = m + n

firstEq :: Char -> String -> Bool
firstEq c1 (c2:_) = c1 == c2
firstEq _ _ = False

-- ### et plus généralement on considère :

-- h :: a -> b -> c

-- ## Hors contexte, on peut :

-- ## 1) appliquer la fonction "binaire" à deux arguments :

-- >>> :t plus 34 8
-- plus 34 8 :: Int

-- >>> plus 34 8
-- 42

-- >>> :t firstEq 'a' "abc"
-- firstEq 'a' "abc" :: Bool

-- >>> firstEq 'a' "abc"
-- True

-- ### Plus généralement :  si x :: a et y :: b,
-- ### alors h x y :: c

--------------------------------------------------------------------------
-- ## 2) faire une application partielle à un argument :

-- >>> :t plus 34
-- plus 34 :: Int -> Int

-- que l'on peut appliquer ensuite :

-- >>> (plus 34) 8
-- 42

-- de même :

-- >>> :t firstEq 'a'
-- firstEq 'a' :: String -> Bool

-- >>> (firstEq 'a') "abc"
-- True

-- ### Plus généralement :  si x :: a,
-- ### alors h x :: b -> c
-- ### et (h x) y :: c   si y :: b

--------------------------------------------------------------------------
-- # Fonctions binaires dans un contexte ?

-- ### On peut pas «facilement» appliquer de fonction binaire
-- ### dans un contexte fonctoriel

-- >>> plus (Just 8) (Just 34)
--     • Couldn't match expected type ‘Int’ with actual type ‘Maybe a0’
--     • In the first argument of ‘plus’, namely ‘(Just 8)’
--       In the expression: plus (Just 8) (Just 34)

-- >>> plus (True, 8) (True, 34)
--     • Couldn't match expected type ‘Int’ with actual type ‘(Bool, b0)’
--     • In the first argument of ‘plus’, namely ‘(True, 8)’
--       In the expression: plus (True, 8) (True, 34)
--       In an equation for ‘it’: it = plus (True, 8) (True, 34)

-- >>> firstEq (Just 'a') (Just "abc")
--     • Couldn't match expected type ‘Char’ with actual type ‘Maybe Char’
--     • In the first argument of ‘firstEq’, namely ‘(Just 'a')’
--       In the expression: firstEq (Just 'a') (Just "abc")
--     • Couldn't match type: Maybe String
--                      with: [Char]
--       Expected: String
--         Actual: Maybe String

-- ### Cependant, on peut effectuer une application partielle grâce à fmap (a.k.a. <$>)

-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- plus: Int -> Int -> Int
--        a  -> (   b     )

-- fmap plus (Just 8) :: Maybe (Int -> Int)
--              ----
--            Maybe Int
--               f   a

-- >>> :t plus <$> (Just 8)
-- plus <$> (Just 8) :: Maybe (Int -> Int)

-- >>> :t plus <$> Nothing
-- plus <$> Nothing :: Maybe (Int -> Int)

-- >>> :t plus <$> (True, 8)
-- plus <$> (True, 8) :: (Bool, Int -> Int)

-- >>> :t plus <$> [1, 2, 3, 4]
-- plus <$> [1, 2, 3, 4] :: [Int -> Int]

-- >>> :t firstEq <$> (Just 'a')
-- firstEq <$> (Just 'a') :: Maybe (String -> Bool)

-- ### ==> plus généralement :
-- si h :: a -> b -> c  et  x :: f a   pour un contexte f,
-- alors h <$> x :: f (b -> c)

-- ### ... on se retrouve avec une fonction dans le contexte !?
-- ### ==> et on est en quelque sort bloqué ...

--------------------------------------------------------------------------
-- # Heureusement il y a ...

-- (image de Lambda-superman, très drôle si si !)

-- # ... les foncteurs applicatifs !

--------------------------------------------------------------------------
-- # Foncteurs applicatifs (a.k.a. «Applicatives»)

-- ## Rappel du «blocage» :

-- >>> :t plus <$> (Just 8)
-- plus <$> (Just 8) :: Maybe (Int -> Int)

-- >>> :t firstEq <$> (Just 'a')
-- firstEq <$> (Just 'a') :: Maybe (String -> Bool)

-- ## Plus généralement :

-- ### si pour un contexte f :: * -> *
-- h :: a -> b -> c
-- x :: f a
-- ### alors
-- h <$> x :: f (b -> c)

-- ### On aimerait alors quelque chose du type :

-- f (b -> c) -> f b -> f c

-- ### Si on cherche cette signature dans Hoogle, on trouve :

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- ### C'est la typeclass Applicative!

--------------------------------------------------------------------------
-- # La typeclass Applicative

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   {-# MINIMAL pure, ((<*>) | liftA2) #-}
--   	-- Defined in ‘GHC.Base’
-- instance [safe] Applicative List
--   -- Defined at /tmp/dantesoXSgj.hs:652:10
-- instance [safe] Applicative MyMaybe
--   -- Defined at /tmp/dantesoXSgj.hs:536:10
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Applicative ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) =>
--          Applicative ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance Applicative ((->) r) -- Defined in ‘GHC.Base’
-- instance Applicative IO -- Defined in ‘GHC.Base’
-- instance Applicative [] -- Defined in ‘GHC.Base’
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Applicative Solo -- Defined in ‘GHC.Base’
-- instance Control.Arrow.Arrow a => Applicative (WrappedArrow a b)
--   -- Defined in ‘Control.Applicative’
-- instance Monad m => Applicative (WrappedMonad m)
--   -- Defined in ‘Control.Applicative’
-- instance Applicative ZipList -- Defined in ‘Control.Applicative’
-- instance Applicative (Either e) -- Defined in ‘Data.Either’
-- instance Monoid m => Applicative (Const m)
--   -- Defined in ‘Data.Functor.Const’

-- ### • pure permet d'injecter une valeur dans le contexte  (arité 0)

-- ### • <*> se nomme «apply»  pour l'«application contextuelle»  (arité >1)

-- ### • <$> correspond à l'arité 1

--------------------------------------------------------------------------
-- # Les lois applicatives

-- ## 1) Loi d'identité

-- pure id <*> v = v

-- ## 2) Loi dite d'homomorphisme

-- pure g <*> pure x = pure (g x)

-- ## 3) Loi dite d'interchange

-- u <*> pure y = pure ($ y) <*> u

-- ### On rappelle la signature de `$` :

-- >>> :t ($)
-- ($) :: (a -> b) -> a -> b

-- ### On peut donc réécrire cette loi de la façon suivante :

-- u <*> pure y = pure (\f -> f y) <*> u

-- ## 4) Loi fonctorielle :

-- f <$> x = pure f <*> x

-- Cette dernière loi indique précisément en quoi un foncteur applicatif est également un
-- foncteur "simple".

-- ### Exercice : écrire ces lois sous forme de propriétés Haskell

--------------------------------------------------------------------------
-- # Exemple 1 : avec Maybe

-- ### On souhaitait effectuer des applications contextuelles de la forme

-- plus (Just 8) (Just 34)  -- erreur

-- firstEq (Just 'a') (Just "abc") -- erreur

-- ### Comme Maybe est instance de Functor on a un peu progressé :

-- >>> :t plus <$> (Just 8)
-- plus <$> (Just 8) :: Maybe (Int -> Int)

-- >>> :t firstEq <$> (Just 'a')
-- firstEq <$> (Just 'a') :: Maybe (String -> Bool)

-- ### Comme Maybe est aussi instance de Applicative
-- ### ==> avec «apply» on peut maintenant continuer

-- >>> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- >>> :t plus <$> (Just 8) <*> (Just 34)
-- plus <$> (Just 8) <*> (Just 34) :: Maybe Int

-- >>> plus <$> (Just 8) <*> (Just 34)
-- Just 42

-- >>> plus <$> Nothing <*> (Just 34)
-- Nothing

-- >>> plus <$> (Just 8) <*> Nothing
-- Nothing

-- >>> :t firstEq <$> (Just 'a') <*> (Just "abc")
-- firstEq <$> (Just 'a') <*> (Just "abc") :: Maybe Bool

-- >>> firstEq <$> (Just 'a') <*> (Just "abc")
-- Just True

--------------------------------------------------------------------------
-- # Exemple 2 : avec les paires et les listes

-- ## On a :

-- ## Applicative []

-- >>> plus <$> [1, 2, 3, 4] <*> [10, 20, 30]
-- [11,21,31,12,22,32,13,23,33,14,24,34]

-- ## Monoid m => Applicative () m

-- >>> plus <$> (True, 8) <*> (False, 34)
-- <interactive>:149:21-23: error: [GHC-39999]
--     • No instance for ‘Monoid Bool’ arising from a use of ‘<*>’
--     • In the expression: plus <$> (True, 8) <*> (False, 34)
--       In an equation for ‘it’: it = plus <$> (True, 8) <*> (False, 34)

-- >>> plus <$> ("hello ", 8) <*> ("world!", 34)
-- ("hello world!",42)

-- >>> plus <$> ([1, 2, 3], 8) <*> ([4, 5, 6, 7], 34)
-- ([1,2,3,4,5,6,7],42)


--------------------------------------------------------------------------
-- # Arités des appels contextuels

-- ### Les foncteurs applicatifs couvrent toutes les arités

-- ## 1) les constantes (arité 0)

-- >>> pure 42 :: Maybe Int
-- Just 42

-- >>> pure True :: (String, Bool)
-- ("",True)

-- ### Pour f un foncteur applicatif:  pure f :: a -> f a

-- ## 2) les fonctions unaires (arité 1)

-- >>> (+1) <$> (Just 1)
-- Just 2

-- >>> id <$> (True, 3)
-- (True,3)

-- >>> (\x -> -x) <$> [1, 2, 3, 4]
-- [-1,-2,-3,-4]

-- ### Pour f un foncteur applicatif, g :: a -> b et x :: f a
-- ### ==> g <$> x :: f b

-- ## 3) les fonctions binaires (arité 2)

-- >>> (+) <$> (Just 8) <*> (Just 34)
-- Just 42

-- >>> (+) <$> ("hello ", 8) <*> ("world!", 34)
-- ("hello world!",42)

-- >>> firstEq <$> (Just 'a') <*> (Just "abc")
-- Just True

-- >>> (<>) <$> [[1, 2], [3, 4, 5]] <*> [[6], [7, 8], [9, 10, 11]]
-- [[1,2,6],[1,2,7,8],[1,2,9,10,11],[3,4,5,6],[3,4,5,7,8],[3,4,5,9,10,11]]

-- ## 4) et les arités > 2 ?

max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

-- >>> :t max3 <$> (Just 3)
-- max3 <$> (Just 3) :: Maybe (Int -> Int -> Int)

-- >>> :t max3 <$> (Just 3) <*> (Just 7)
-- max3 <$> (Just 3) <*> (Just 7) :: Maybe (Int -> Int)

-- >>> max3 <$> (Just 3) <*> (Just 7) <*> (Just 5)
-- Just 7

-- >>> max3 <$> ("bla", 9) <*> ("bli", 7) <*> ("blo", 5)
-- ("blabliblo",9)

--------------------------------------------------------------------------
-- # Forme générale des applications contextuelles

-- ### Soit
-- g :: a1 -> a2 -> ... -> aN -> b
-- ### une fonction N-aire  (N >=1 )

-- ## Une application «classique» (hors-contexte) s'écrit :

-- g  x1  x2  ... xN

-- ### avec
-- x1::a1, x2::a2, ..., xN::aN

-- ## Une application contextuelle
-- ## pour le contexte applicatif f :: * -> *
-- ## s'écrit :

-- g <$> y1 <*> y2 <*> ... <*> yN

-- ### avec
-- y1::f a1, y2::f a2, ..., yN::f aN

-- ### ==> Syntaxiquement, on a quasiment retrouvé la forme  «classique» des applications
-- ### (et donc la programmation fonctionnelle)

--------------------------------------------------------------------------
-- # Instances de Applicative

-- Pour illustrer les instantiations de Applicative,
-- on va considérer :

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq, Ord)

data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------
-- # Etape 1 : instanciation de Functor

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing = MyNothing
  fmap g (MyJust x) = MyJust $ g x

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap g (Cons x xs) = Cons (g x) $ fmap g xs

--------------------------------------------------------------------------
-- # Etape 2 : instanciation de Applicative

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- ### pure injecte une valeur «simple» dans le contexte

pureMyMaybe :: a -> MyMaybe a
pureMyMaybe = MyJust

pureList :: a -> List a
pureList x = Cons x Nil

-- ### Question: existe-t-il une autre possibilité ? (cf. TD)

--------------------------------------------------------------------------
-- # Implémentation de <*>

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b


applyMyMaybe :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
applyMyMaybe (MyJust g) (MyJust x) = MyJust (g x)
applyMyMaybe _ _ = MyNothing

-- >>> applyMyMaybe (MyJust (+1)) (MyJust 4)
-- MyJust 5

-- >>> applyMyMaybe (MyJust (+1)) MyNothing
-- MyNothing

-- ## Instanciation de Applicative pour MyMaybe

instance Applicative MyMaybe where
  pure = pureMyMaybe
  (<*>) = applyMyMaybe

-- >>> (+) <$> MyJust 8 <*> MyJust 34
-- MyJust 42

--------------------------------------------------------------------------
-- # et pour les listes ?

-- >>> :t (+) <$> [1, 2, 3] <*> [4, 5]

-- ### On peut imaginer le résultat :
-- [5, 7]

-- ### mais en pratique :

-- >>> (+) <$> [1, 2, 3] <*> [4, 5]

-- ce qui correspond à :

-- >>> [(x+y) | x <- [1, 2, 3], y <- [4, 5]]

-- ### mais on a aussi :

-- >>> :info ZipList
-- type ZipList :: * -> *
-- newtype ZipList a = ZipList {getZipList :: [a]}
--   	-- Defined in ‘Control.Applicative’

-- >>> (+) <$> (ZipList [1, 2, 3]) <*> (ZipList [4, 5])

-- ### ==> Contrairement aux Foncteurs, il y a souvent 
-- ### plusieurs interprétations possibles des Applicatives

-- ### ==> et comme on a au plus une instance par type,
-- ### on utilise `newtype` pour pouvoir implémenter
-- ### des alternatives  (sans impact au runtime)

--------------------------------------------------------------------------
-- # Exemple : Listes «maison» applicatives

-- ### On interprète nos listes applicatives comme des «compréhensions»

-- ### On a déjà pure :

-- >>> :t pureList
-- pureList :: a -> List a

-- Pour  «apply», la signature est la suivante :

applyList :: List (a -> b) -> List a -> List b

-- ### L'objectif, dans (applyList gs xs), est :
-- ### pour chaque fonction g de gs :
-- ### => appliquer la fonction g à tous les éléments de xs

-- Avec g :: a -> b  et  xs :: List a
-- On veut :: List (List b)

-- Solution: fmap g xs

-- >>> fmap even [1, 2, 3]
-- [False,True,False]

-- >>> fmap even (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons False (Cons True (Cons False Nil))

-- ### Donc on pense, pour gs, à :

-- fmap (\g -> fmap g xs) gs

-- >>> fmap (\g -> fmap g [1, 2, 3]) [even, odd]
-- [[False,True,False],[True,False,True]]

-- >>> fmap (\g -> fmap g (Cons 1 (Cons 2 (Cons 3 Nil)))) (Cons even (Cons odd Nil)) 
-- Cons (Cons False (Cons True (Cons False Nil))) (Cons (Cons True (Cons False (Cons True Nil))) Nil)

-- >>> fmap (\g -> fmap g [1, 2, 3]) [(+1), (\x->x-3), (*2)]
-- [[2,3,4],[-2,-1,0],[2,4,6]]

-- >>> fmap (\g -> fmap g (Cons 1 (Cons 2 (Cons 3 Nil)))) (Cons (+1) (Cons (\x -> x-3) (Cons (*2) Nil))) 
-- Cons (Cons 2 (Cons 3 (Cons 4 Nil))) (Cons (Cons (-2) (Cons (-1) (Cons 0 Nil))) (Cons (Cons 2 (Cons 4 (Cons 6 Nil))) Nil))

-- ### Mais on n'a pas fini, car il faut ensuite concaténer les listes obtenues
-- ### (sinon on aura un type (List (List b)) plutôt que (List b)

-- >>> (concat . fmap (\g -> fmap g [1, 2, 3])) [(+1), (\x->x-3), (*2)]
-- [2,3,4,-2,-1,0,2,4,6]

-- ### Il existe une fonction connue qui réalise ces opérations.
-- ### Sa signature est la suivante :

-- (a -> [b]) -> [a] -> [b]
-- ### Dans Hoogle on trouve :

-- >>> :t concatMap
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- >>> concatMap (\g -> fmap g [1, 2, 3]) [(+1), (\x->x-3), (*2)]
-- [2,3,4,-2,-1,0,2,4,6]

--------------------------------------------------------------------------
-- # concatMap pour les listes «maison»

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x $ appendList xs ys

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons xs yys) = xs `appendList` (concatList yys)

concatMapList :: (a -> List b) -> List a -> List b
concatMapList g = concatList . fmap g

-- ## et donc on obtient notre «apply» :

-- ### applyList :: List (a -> b) -> List a -> List b
applyList gs xs = concatMapList (\g -> fmap g xs) gs

-- ## ... et notre implémentation de Applicative

instance Applicative List where
  pure = pureList
  (<*>) = applyList

-- ### Rappel :

-- >>> (+) <$> [1, 2, 3] <*> [4, 5]
-- [5,6,6,7,7,8]

-- >>> (+) <$> (Cons 1 (Cons 2 (Cons 3 Nil))) <*> (Cons 4 (Cons 5 Nil))
-- Cons 5 (Cons 6 (Cons 6 (Cons 7 (Cons 7 (Cons 8 Nil)))))

--------------------------------------------------------------------------
-- # et pour les listes conteneurs ?
-- (a.k.a. ZipList)

-- >>> (+) <$> (ZipList [1, 2, 3]) <*> (ZipList [4, 5])
-- ZipList {getZipList = [5,7]}

-- ## ===> cf. exercice de TD

--------------------------------------------------------------------------
-- # Application : les structures traversables

-- Il existe un certain nombre d'applications pratiques des foncteurs applicatifs,
-- dont nous verrons quelques exemple en TD et TME.

-- ### Une application intéressante concerne l'extension des structures foldables
-- ### aux foncteurs applicatifs

-- ## ==> les structures traversables

-- ## Objectif : effectuer des folds sur des structures (foldables) qui
-- ##            contiennent des valeurs contextuelles.

-- ## Exemple : filtrage de listes de valeurs optionnelles

-- ### Considérons le problème suivant :

-- Supposons des liste contenant des entiers avec la contrainte que
-- tous les éléments soient positifs.

-- ### Par exemple :

-- • la liste [3, 9, 0, 12, 2, 1]  doit être retenue

-- • la liste [2, 5, -3, 0, 9, -4, 8, -12, 3] doit être rejetée.

-- ### La signature qui nous intéresse est la suivante :

-- (Num a, Ord a) => [a] -> Maybe [a]

--------------------------------------------------------------------------
-- # Version 1 en style direct

allPositives :: (Num a, Ord a) => [a] -> Maybe [a]
allPositives [] = Just []
allPositives (x:xs) = case allPositives xs of
                        Nothing -> Nothing
                        Just xs' -> if x >= 0 then Just (x:xs')
                                    else Nothing

-- >>> allPositives [3, 9, 0, 12, 2, 1]
-- Just [3,9,0,12,2,1]

-- >>> allPositives [2, 5, -3, 0, 9, -4, 8, -12, 3]
-- Nothing

--------------------------------------------------------------------------

-- # Version 2 en style point-free

-- 1) injecter les valeurs optionelles

keepPositive :: (Num a, Ord a) => a -> Maybe a
keepPositive x | x >= 0 = Just x
               | otherwise = Nothing


-- >>> fmap keepPositive [3, 9, 0, 12, 2, 1]
-- [Just 3,Just 9,Just 0,Just 12,Just 2,Just 1]

-- >>> fmap keepPositive [2, 5, -3, 0, 9, -4, 8, -12, 3]
-- [Just 2,Just 5,Nothing,Just 0,Just 9,Nothing,Just 8,Nothing,Just 3]

-- 2) retournement de la liste

extractJust :: [Maybe a] -> Maybe [a]  -- <== signature à remarquer !
extractJust [] = Just []
extractJust (Nothing:_) = Nothing
extractJust (Just y:xs) = case extractJust xs of
                            Nothing -> Nothing
                            Just ys -> Just (y:ys)

-- et tout ce qu'il faut pour le point-free :

allPositives' :: (Num a, Ord a) => [a] -> Maybe [a]
allPositives' = extractJust . fmap keepPositive

-- >>> allPositives' [3, 9, 0, 12, 2, 1]
-- Just [3,9,0,12,2,1]

-- >>> allPositives' [2, 5, -3, 0, 9, -4, 8, -12, 3]
-- Nothing

-- ## Q: la fonction `extractJust` n'est-elle pas encore généralisable ?


--------------------------------------------------------------------------
-- # Versions "foldées"

-- ## Rappel : les 3 variantes de fold

--  >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- pour les listes:       (a -> b -> b) -> b -> [a] -> b

-- >>> :t foldl'
-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- pour les listes :       (b -> a -> b) -> b -> [a] -> b

-- ### Nous avons aussi vu une généralisation, toujours sur les *foldables* :

-- >>> :t foldMap
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- pour les listes :                    (a -> m) -> [a] -> m


selectPositive :: (Num a, Ord a) => Maybe [a] -> a -> Maybe [a]
selectPositive Nothing _ = Nothing
selectPositive (Just xs) x = if x >= 0 then Just (x:xs) else Nothing

allPositivesFL :: (Num a, Ord a) => [a] -> Maybe [a]
allPositivesFL = foldl' selectPositive (Just [])

allPositivesFR :: (Num a, Ord a) => [a] -> Maybe [a]
allPositivesFR = foldr (flip selectPositive) (Just [])

-- >>> :t flip

-- >>> allPositivesFL [3, 9, 0, 12, 2, 1]
-- Just [1,2,12,0,9,3]

-- >>> allPositivesFL [2, 5, -3, 0, 9, -4, 8, -12, 3]
-- Nothing

-- >>> allPositivesFR [3, 9, 0, 12, 2, 1]
-- Just [3,9,0,12,2,1]

-- >>> allPositivesFR [2, 5, -3, 0, 9, -4, 8, -12, 3]
-- Nothing

-- ## Exercice : une version avec foldMap ?

--------------------------------------------------------------------------
-- # Version "traversable"

-- ### Avec les foncteurs applicatifs et les structures *traversables*,
-- ### nous avons une alternative intéressante.

-- ... retour à la version décomposée

-- Rappel :

-- >>> :t keepPositive
-- keepPositive :: (Num a, Ord a) => a -> Maybe a

-- ### Avec `fmap` on obtient un résultat presque satisfaisant :

-- >>> fmap keepPositive [3, 9, 0, 12, 2, 1]
-- [Just 3,Just 9,Just 0,Just 12,Just 2,Just 1]


-- >>> fmap keepPositive [2, 5, -3, 0, 0, -4, 8, -12, 3]
-- [Just 2,Just 5,Nothing,Just 0,Just 0,Nothing,Just 8,Nothing,Just 3]

-- et on avait ensuite utilisé la fonction `extractJust` qui
-- permet d'inverser les deux contextes: 

-- >>> :t extractJust
-- extractJust :: [Maybe a] -> Maybe [a]

-- ### Dans le cadre d'un fold, c'est légèrement différent :
-- ### on aimerait bien *folder* les `Maybe`'s contenus dans
-- ### la structure pour en déduire le `Maybe` qui couvre la structure.

-- ### Ce type de *fold* est exactement ce que réalise la fonction générique `traverse`
-- ### dont voici le type :

-- >>> :t traverse
-- traverse
--   :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- ### ... qui provient de la typeclass Traversable

-- >>> :info Traversable

-- ### Si on spécialise cette signature sur les listes (pour le conteneur "traversable")
-- ### et les `Maybe`(pour le contexte fonctoriel applicatifs), alors on obtient la
-- ### signature suivante :

-- traverse :: ... => (a -> Maybe b) -> [a] -> Maybe [b]

-- >>> traverse keepPositive [3, 9, 0, 12, 2, 1]
-- Just [3,9,0,12,2,1]


-- >>> traverse keepPositive [2, 5, -3, 0, 0, -4, 8, -12, 3]
-- Nothing

-- ### Plus généralement, l'idée est de traverser la structure en foldant
-- ### le contenu avec le foncteur applicatif concerné.

--------------------------------------------------------------------------
-- # SequenceA et fmap

-- >>> :info Traversable
-- type Traversable :: (* -> *) -> Constraint
-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   sequence :: Monad m => t (m a) -> m (t a)
--   {-# MINIMAL traverse | sequenceA #-}

-- ### Si l'on reprend la solution partielle obtenue avec fmap :

-- >>> fmap keepPositive [3, 9, 0, 12, 2, 1]
-- [Just 3,Just 9,Just 0,Just 12,Just 2,Just 1]

-- >>> fmap keepPositive [2, 5, -3, 0, 0, -4, 8, -12, 3]
-- [Just 2,Just 5,Nothing,Just 0,Just 0,Nothing,Just 8,Nothing,Just 3]

-- On se posait la question de la généralisation de `extractJust` :

-- >>> :t extractJust

-- ### ==> Cette généralisation se nomme: sequenceA

-- >>> sequenceA $ fmap keepPositive [3, 9, 0, 12, 2, 1]
-- Just [3,9,0,12,2,1]

-- >>> sequenceA $ fmap keepPositive  [2, 5, -3, 0, 0, -4, 8, -12, 3]
-- Nothing

--------------------------------------------------------------------------
-- # Exercice : traverse vs. sequenceA

-- ## (1) traverse en fonction de sequenceA

traverse' :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' g = sequenceA . fmap g

-- ## (2) sequenceA en fonction de traverse

sequenceA' :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA' = traverse id -- f a -> f a

--------------------------------------------------------------------------
-- # Listes «maison» traversables

-- Etudions l'instanciation de la typeclasse Traversable
-- sur un cas pratique.

-- Rappel :

-- >>> :info Traversable

-- ## Etape 1 : il faut être foldable (cf. cours précédent)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap m (Cons x xs) = (m x) <> (foldMap m xs)

-- ## Etape 2 : implémentation de `traverse` sur le schéma de `fmap`  

-- ### Rappel :
-- fmap :: Functor f => (a -> b) -> List a -> List b
-- fmap _ Nil = Nil
-- fmap g (Cons x xs) = Cons (g x) (fmap g xs)

-- ### ... pour traverse la structure est similaire, mais avec
-- ### des appels contextuels

traverseList :: Applicative f => (a -> f b) -> List a -> f (List b)
traverseList _ Nil = pure Nil
traverseList g (Cons x xs) = Cons <$> (g x) <*> (traverseList g xs)

-- Et cela fonctionne comme prévu :

-- >>> traverseList keepPositive (Cons 3 (Cons 9 (Cons 0 (Cons 12 (Cons 2 (Cons 1 Nil))))))

-- >>> traverseList keepPositive (Cons 2 (Cons 5 (Cons (-3) (Cons 0 (Cons 0 (Cons (-4) (Cons 8 (Cons (-12) (Cons 3 Nil)))))))))

--------------------------------------------------------------------------
-- # Instanciation de Traversable

instance Traversable List where
  traverse = traverseList

-- >>> traverse keepPositive (Cons 3 (Cons 9 (Cons 0 (Cons 12 (Cons 2 (Cons 1 Nil))))))

-- >>> traverse keepPositive (Cons 2 (Cons 5 (Cons (-3) (Cons 0 (Cons 0 (Cons (-4) (Cons 8 (Cons (-12) (Cons 3 Nil)))))))))

-- ### et donc

allPositives'' :: (Num a, Ord a) => List a -> Maybe (List a)
allPositives'' = traverse keepPositive

-- >>> allPositives'' (Cons 3 (Cons 9 (Cons 0 (Cons 12 (Cons 2 (Cons 1 Nil))))))

-- >>> allPositives'' (Cons 2 (Cons 5 (Cons (-3) (Cons 0 (Cons 0 (Cons (-4) (Cons 8 (Cons (-12) (Cons 3 Nil)))))))))


--------------------------------------------------------------------------
-- # Conclusion

-- Les foncteurs applicatifs ont été "découverts" et étudiés assez récemment
-- ### Applicative Programming with Effects
-- ### Conor McBride and Ross Paterson
-- ### Journal of Functional Programming 18:1 (2008)

-- Il s'agit, en quelque sorte, d'un "chaînon manquant" entre :
-- - les foncteurs (du cours précédent)
-- - et les monades (du prochain cours)

-- Mettre des fonctions (type `a -> b`) dans des contextes (notamment des listes)
-- peut apparaître comme un peu contre-intuitif,  mais on retiendra simplement :

-- un appel hors-contexte s'écrit :
-- ### f x1 x2 ... xN

-- ... alors qu'un appel contextuel s'écrit :
-- ### g <$> x1 <*> x2 <*> ... <*> xN


-- En pratique, en complément de permettre le style
-- fonctionnel "en contexte", les foncteurs applicatifs sont très utilisés

-- - dans le parsing dit "applicatif"  (cf. optparse, regex-applicative, etc.)

-- - dans les structures taversables pour notamment la gestion des valeurs optionnelles (Maybe)
-- ou des erreurs (Either e) dans des structures traversables.

-- ==> cf. exercice de TD "validation"
