
{-# LANGUAGE InstanceSigs #-}

module Cours6_Script where

-- Quelques modules standards
-- associés au cours
import Data.Functor
import Data.Semigroup
import Data.Monoid

-- Pour illustrer la Typeclass Foldable
import Data.Foldable (Foldable)
import qualified Data.Foldable as F


-------------------------------------------------------------------------
-- # PAF 6 : Structures algébriques (1/3)

-- ## - Prélude : la notion de catégorie

-- ## - Foncteurs

-- ## - Semi-groupes et monoïdes

-- ## - Application : Structures foldables

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)



-------------------------------------------------------------------------
-- # Prélude : la notion de catégorie

-- La théorie des catégories n'est pas un prérequis pour la programmation
-- fonctionnelle, mais connaître quelques rudiments est très utile.
-- ==> la plupart des concepts dits «algébriques» que nous allons étudier
-- proviennent de la théorie des catégories

-- ## Catégorie = structure de «choses» qui se composent bien

-- ## Un peu plus formellement ...
-- (cf. document externe)

-- ## Quelques exemples :

-- ## La catégorie Set  (Large)
-- - les objets sont des ensembles
-- - les morphismes sont des fonctions totales
-- - identités et composition usuelles

-- ## Une catégorie PO(E, <=)  (Small)
-- - les objets sont des éléments d'un ensemble  E
-- - chaque morphisme a --> b  correspond à une relation  a <= b
-- - identité = réflexivité
-- - composition = transitivité

-------------------------------------------------------------------------
-- # La catégorie Hask

-- On considère le sous-ensemble de Haskell composé :

-- - des types dits "saturés" => les objets
-- - des fonctions totales => les morphismes

-- On vérifie les conditions ...

-- ## Identités

-- >>> :t id
-- id :: a -> a


-- ### -- Implémentation possible:
-- id x = x

-- ## Composition

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- ### -- Implémentation possible :
-- (.) g f x = g (f x)

-------------------------------------------------------------------------
-- # Les lois de Hask

-- ## Lois d'identité

-- ### -- en théorie : f . id  = f
law_Cat_rightId :: Eq b => (a -> b) -> a -> Bool
law_Cat_rightId f x = (f . id) x == f x

-- ### Preuve :
-- (f . id) x
-- = f (id x)    { équation de (.) }
-- = f x         { équation de id }
-- (CQFD)

-- ### -- en théorie : id . f = f
law_Cat_leftId :: Eq b => (a -> b) -> a -> Bool
law_Cat_leftId f x = (id . f) x == f x

-- ### Preuve :
-- (id . f) x
-- = id (f x)    { équation de (.) }
-- = f x         { équation de id }
-- (CQFD)

-- ## Lois de composition

-- ### -- en théorie : h . (g . f) = (h . g) . f
law_Cat_assoc :: Eq d => (c -> d) -> (b -> c) -> (a -> b) -> a -> Bool
law_Cat_assoc h g f x = (h . (g . f)) x == ((h . g) . f) x

-- ### Preuve :
-- (h . (g . f) x)
-- = h ((g . f) x)   { équation de (.) }
-- = h (g (f x))     { équation de (.) }
-- = (h . g) (f x)   { équation de (.) <- }
-- = ((h . g) . f) x  { équation de (.) <- }
-- (CQFD)

-------------------------------------------------------------------------  
-- # Foncteurs

-- Le premier concept de théorie des catégories que nous
-- étudions est le foncteur.

-- ## Définition
-- (cf. figure annexe)

-- En Haskell, la définition correspondante est la suivante :

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--   	-- Defined in ‘GHC.Base’
-- instance [safe] Functor Box
--   -- Defined at /tmp/dantecK1ovJ.hs:231:10
-- instance [safe] Functor MyMaybe
--   -- Defined at /tmp/dantecK1ovJ.hs:340:10
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,,) a b c d) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,,,) a b c d e) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,,,,) a b c d e f) -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor Solo -- Defined in ‘GHC.Base’
-- instance Functor (Arg a) -- Defined in ‘Data.Semigroup’
-- instance Functor Data.Semigroup.First
--   -- Defined in ‘Data.Semigroup’
-- instance Functor Data.Semigroup.Last -- Defined in ‘Data.Semigroup’
-- instance Functor Max -- Defined in ‘Data.Semigroup’
-- instance Functor Min -- Defined in ‘Data.Semigroup’
-- instance Functor f => Functor (Ap f) -- Defined in ‘Data.Monoid’
-- instance Functor Data.Monoid.First -- Defined in ‘Data.Monoid’
-- instance Functor Data.Monoid.Last -- Defined in ‘Data.Monoid’
-- instance Functor f => Functor (Alt f)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Functor Dual
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Functor Product
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Functor Sum
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Functor (Either a) -- Defined in ‘Data.Either’

-- ## Ici, f se nomme : le contexte fonctoriel

-- On retrouve les propriétés théoriques :

-- ### - La catégorie de départ est Hask

-- ==> les objets sont des types a, b, ...
-- ==> les morphismes sont des fonctions g :: a -> b, h :: b -> c, ...

-- ### - La catégorie d'arrivée est (f Hask)

-- ==> les objets sont des types (f a),  (f b), ...

-- ==> les morphismes sont les fonctions "projetées"

-- Si g :: a -> b    alors   fmap g :: f a -> f b

-- ## Les lois fonctorielles en Haskell

-- ### -- en théorie :  fmap id = id
law_Functor_id :: (Functor f, Eq (f a)) => f a -> Bool
law_Functor_id x = (fmap id x) == x

-- ### -- en théorie : fmap (g . f) = fmap g . fmap f
law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
law_Functor_comp h g x = fmap (h . g) x == (fmap h . fmap g) x

-------------------------------------------------------------------------
-- # Contexte fonctoriel ?

-- On a vu jusqu'à présent essentiellement des contraintes sur les types:
-- => Show Int,  Eq Bool,  Ord [Int],  etc.

-- ## Functor est une contrainte d'ordre supérieur
-- ## avec le kind: * -> *

-- (en anglais Higher-kinded constraint)

-- ### Ce kind * -> * représente les constructeurs de type à un argument
-- ### (comme Maybe, [], etc.)

-- Pour qu'un constructeur de type devienne un contexte fonctoriel, il faut:

-- 1) instancier la typeclasse Functor (donc implémenter fmap)
-- 2) vérifier les lois fonctorielles

-- Il y a schématiquement deux styles de contexte fonctoriels

-- - les contextes structurels,
--   a.k.a.  conteneurs (containers)

-- - les contextes comportementaux (computational),
--   a.k.a. contextes «tout court»

-------------------------------------------------------------------------
-- # Contexte structurel : exemple

-- Le plus simple des conteneurs est «la boîte» que l'on peut définir ainsi :

data Box a = Box a
  deriving (Show, Eq)

-- ### Vérifions le kind de Box :

-- >>> :k Box
-- Box :: * -> *

-- ### ==> Il s'agit donc bien d'un candidat à la «fonctorisation»

-------------------------------------------------------------------------
-- # Etape 1 : instanciation de Functor

-- >>> :info Box

-- La signature à implémenter est la suivante :

boxMap :: (a -> b) -> Box a -> Box b
-- -- une seule implémentation possible
boxMap g (Box v) = Box (g v)

-- >>> boxMap (+1) (Box 4)
-- Box 5

-- ### On peut maintenant finaliser l'étape 1

instance Functor Box where
  fmap = boxMap

-- >>> fmap (+1) (Box 4)
-- Box 5

-- >>> fmap show (Box 4)
-- Box "4"

-- ## Intuitivement

-- ### Si b est une boîte, alors :

-- fmap g b

-- ### ... applique g «à l'intérieur» de b

-------------------------------------------------------------------------
-- # Etape 2 : vérification des lois

-- ## Loi d'identité

-- law_Functor_id :: (Functor f, Eq (f a)) => f a -> Bool
-- law_Functor_id x = (fmap id x) == x

-- ### Preuve (pour Box) :
-- fmap id x
-- = fmap id (Box v)      { structure de x }
-- = boxMap id (Box v)    { instance de Functor }
-- = Box (id v)           { équation de boxMap }
-- = x
-- (CQFD) 

-- ## Loi de composition

-- law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
-- law_Functor_comp h g x = fmap (h . g) x == (fmap h . fmap g) x

-- ### Preuve (pour Box) :
-- On a :
-- (fmap h . fmap g) x
-- = (fmap h . fmap g) (Box v) { structure de x }
-- = fmap h (fmap g (Box v)    { équation de (.) }
-- = fmap h (boxMap g (Box v)) { instance de Functor }
-- = fmap h (Box (g v))        { équation de boxMap }
-- = boxMap h (Box (g v))      { instance de Functor }
-- = Box (h (g v))             { équation de boxMap }

-- et :
-- fmap (h . g) x
-- = fmap (h . g) (Box v)    { structure de x }
-- = boxMap (h . g) (Box v)  { instance de Functor }
-- = Box ((h . g) v)         { équation de boxMap }
-- = Box (h (g v))           { équation de (.) }

-- (CQFD)

-------------------------------------------------------------------------
-- # Autres contextes structurels ...

-- (en exercice)

-- ### - Les paires, (triplets, etc.) :

data LPair a b = LPair a b

data RPair b a = RPair b a

-- Quelle définition choisir ?

-- cf. les paires standards  (triplets, ...) ?

-- >>> fmap (+1) (41, True)

-- ou ?

-- >>> fmap (+1)  (True, 41)

-- ### - Les records paramétrés :

data Record a = Record { entier :: Int, chaine :: String, val :: a }

-- ### - Les séquences  (Seq a)

-- ... etc ...


-------------------------------------------------------------------------
-- # Contexte calculatoire : exemple

-- Les foncteurs ne s'appliquent pas uniquement à des structures

-- ## Exemple : le contexte des calculs optionnels

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq)

-- Cette fois-ci on doit implémenter la signature suivante :

myMaybeMap :: (a -> b) -> MyMaybe a -> MyMaybe b
-- -- Remarque : une seule implémentation «raisonnable» possible
myMaybeMap _ MyNothing = MyNothing
myMaybeMap g (MyJust v) = MyJust (g v)

-------------------------------------------------------------------------
-- # Etape 1 : instancation de Functor

instance Functor MyMaybe where
  fmap = myMaybeMap

-- >>> fmap (+1) MyNothing
-- MyNothing

-- >>> fmap (+1) (MyJust 41)
-- MyJust 42

-- ### Intuitivement :  si b est un calcul optionel
-- fmap g b  applique g au calcul optionnel

-- ### Remarque : le Maybe du prélude se comporte de la même façon

-- >>> fmap (+1) Nothing
-- Nothing

-- >>> fmap (+1) (Just 41)
-- Just 42

-------------------------------------------------------------------------
-- # Etape 2 : vérification des lois

-- law_Functor_id :: (Functor f, Eq (f a)) => f a -> Bool
-- law_Functor_id x = (fmap id x) == x

-- ### Preuve :
-- fmap id x
-- = myMaybeMap id x          { instance de Functor }

-- (cas 1)
-- = myMaybeMap id MyNothing  { structure de x, cas MyNothing }
-- = MyNothing                { équation myMaybeMap.1 }
-- = x

-- (cas 2)
-- = myMaybeMap id (MyJust v) { structure de x, cas MyJust }
-- = MyJust (id v)            { équation myMaybeMap.2 }
-- = MyJust v                 { équation id }
-- = x
-- (CQFD)

-- law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
-- law_Functor_comp h g x = fmap (h . g) x == (fmap h . fmap g) x

-- ### Preuve :
-- (exercice)


-------------------------------------------------------------------------
-- # Autres contextes calculatoires ...

-- (en exercice)

-- ## - Les calculs avec erreurs

data MyEither b a =
  MyLeft b
  | MyRight a

-- Question : pourquoi les paramètres dans cet ordre ?


-- ## - Les calculs non-déterministes avec les listes

data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- Remarque : il existe un second contexte calculatoire possible :
--      ==> les ZipLists  (cf. cours 7)

-- ## - Les calculs avec backtrack

data Tree a =
  Solution a
  | Branch [Tree a]


-- ## - etc.

-------------------------------------------------------------------------

-- # Les semi-groupes et monoïdes

-- ## Définition algébrique d'un semi-groupe

-- ## - un objet/ensemble/type a

-- ## - une fonction («loi») de composition interne (<>) sur les éléments de a

-- ### En Haskell :

-- >>> :info Semigroup
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--   (<>) :: a -> a -> a
--   sconcat :: GHC.Base.NonEmpty a -> a
--   stimes :: Integral b => b -> a -> a
--   {-# MINIMAL (<>) | sconcat #-}
--   	-- Defined in ‘GHC.Base’
-- instance [safe] Semigroup (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:525:10
-- instance Semigroup () -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b) => Semigroup (a, b)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c) =>
--          Semigroup (a, b, c)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
--          Semigroup (a, b, c, d)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d,
--           Semigroup e) =>
--          Semigroup (a, b, c, d, e)
--   -- Defined in ‘GHC.Base’
-- instance Semigroup b => Semigroup (a -> b) -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
-- instance Semigroup [a] -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (Maybe a)
--   -- Defined in ‘GHC.Base’
-- instance Semigroup Ordering -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (Solo a) -- Defined in ‘GHC.Base’
-- instance Semigroup (Data.Semigroup.First a)
--   -- Defined in ‘Data.Semigroup’
-- instance Semigroup (Data.Semigroup.Last a)
--   -- Defined in ‘Data.Semigroup’
-- instance Ord a => Semigroup (Max a) -- Defined in ‘Data.Semigroup’
-- instance Ord a => Semigroup (Min a) -- Defined in ‘Data.Semigroup’
-- instance Monoid m => Semigroup (WrappedMonoid m)
--   -- Defined in ‘Data.Semigroup’
-- instance (Applicative f, Semigroup a) => Semigroup (Ap f a)
--   -- Defined in ‘Data.Monoid’
-- instance Semigroup (Data.Monoid.First a)
--   -- Defined in ‘Data.Monoid’
-- instance Semigroup (Data.Monoid.Last a) -- Defined in ‘Data.Monoid’
-- instance Semigroup All
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance GHC.Base.Alternative f => Semigroup (Alt f a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Semigroup Any
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Semigroup a => Semigroup (Dual a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Semigroup (Endo a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Semigroup (Product a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Semigroup (Sum a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Semigroup (Either a b) -- Defined in ‘Data.Either’

-- ## - une loi (propriété) d'associativité :

law_Semigroup_assoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
law_Semigroup_assoc x y z = x <> (y <> z) == (x <> y) <> z

-------------------------------------------------------------------------
-- # Définition d'un monoïde

-- ## - un semi-groupe
-- ## - avec un élément neutre: mempty

-- ### En Haskell :

-- >>> :info Monoid
-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   {-# MINIMAL mempty | mconcat #-}
--   	-- Defined in ‘GHC.Base’
-- instance [safe] Monoid (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:589:10
-- instance Monoid () -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
--          Monoid (a, b, c, d)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
--          Monoid (a, b, c, d, e)
--   -- Defined in ‘GHC.Base’
-- instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
-- instance Monoid [a] -- Defined in ‘GHC.Base’
-- instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
-- instance Monoid Ordering -- Defined in ‘GHC.Base’
-- instance Monoid a => Monoid (Solo a) -- Defined in ‘GHC.Base’
-- instance (Ord a, Bounded a) => Monoid (Max a)
--   -- Defined in ‘Data.Semigroup’
-- instance (Ord a, Bounded a) => Monoid (Min a)
--   -- Defined in ‘Data.Semigroup’
-- instance Monoid m => Monoid (WrappedMonoid m)
--   -- Defined in ‘Data.Semigroup’
-- instance (Applicative f, Monoid a) => Monoid (Ap f a)
--   -- Defined in ‘Data.Monoid’
-- instance Monoid (Data.Monoid.First a) -- Defined in ‘Data.Monoid’
-- instance Monoid (Data.Monoid.Last a) -- Defined in ‘Data.Monoid’
-- instance Monoid All
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance GHC.Base.Alternative f => Monoid (Alt f a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid Any
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid a => Monoid (Dual a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid (Endo a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Monoid (Product a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Monoid (Sum a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’

-- ## - deux lois de composition avec l'élément neutre

law_Monoid_mempty_left :: (Monoid a, Eq a) => a -> Bool
law_Monoid_mempty_left x = mempty <> x == x

law_Monoid_mempty_right :: (Monoid a, Eq a) => a -> Bool
law_Monoid_mempty_right x = x <> mempty == x

-------------------------------------------------------------------------
-- # Exemple : le monoïde des listes

-- >>> :info List
-- type List :: * -> *
-- data List a = Nil | Cons a (List a)
--   	-- Defined at /tmp/dantecK1ovJ.hs:452:1
-- instance [safe] Eq a => Eq (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:455:19
-- instance [safe] Foldable List
--   -- Defined at /tmp/dantecK1ovJ.hs:721:10
-- instance [safe] Monoid (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:639:10
-- instance [safe] Semigroup (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:632:10
-- instance [safe] Show a => Show (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:455:13

-- La fonction de composition est une concaténation, avec la signature suivante :

concatList :: List a -> List a -> List a
concatList Nil ys = ys
concatList xs Nil = xs
concatList (Cons x xs) ys = Cons x $ concatList xs ys

-------------------------------------------------------------------------
-- # Instanciations de Semigroup et Monoid

instance Semigroup (List a) where
  (<>) = concatList

-- ### Exemple :

-- >>> (Cons 1 (Cons 2 (Cons 3 Nil))) <> (Cons 4 (Cons 5 Nil))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

instance Monoid (List a) where
  mempty = Nil

-- ### Exemples :

-- >>> mempty <> (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 1 (Cons 2 (Cons 3 Nil))

-- >>> (Cons 1 (Cons 2 (Cons 3 Nil))) <> mempty
-- Cons 1 (Cons 2 (Cons 3 Nil))

-------------------------------------------------------------------------
-- # Le monoïde des listes

-- ## Les listes du prélude forment bien sûr un monoïde

-- >>> [1, 2, 3] <> [4, 5]
-- [1,2,3,4,5]

-- >>> mempty <> [1, 2, 3]
-- [1,2,3]

-- >>> [1, 2, 3] <> mempty
-- [1,2,3]


-- ## Exercice
-- Prouver les lois des semi-groupes et monoïde pour (List a) et [a]

-------------------------------------------------------------------------
-- # Autres exemples de Monoïdes :

-- >>> :info Monoid
-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   {-# MINIMAL mempty | mconcat #-}
--   	-- Defined in ‘GHC.Base’
-- instance [safe] Monoid (List a)
--   -- Defined at /tmp/dantecK1ovJ.hs:653:10
-- instance Monoid () -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
--          Monoid (a, b, c, d)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
--          Monoid (a, b, c, d, e)
--   -- Defined in ‘GHC.Base’
-- instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
-- instance Monoid [a] -- Defined in ‘GHC.Base’
-- instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
-- instance Monoid Ordering -- Defined in ‘GHC.Base’
-- instance Monoid a => Monoid (Solo a) -- Defined in ‘GHC.Base’
-- instance (Ord a, Bounded a) => Monoid (Max a)
--   -- Defined in ‘Data.Semigroup’
-- instance (Ord a, Bounded a) => Monoid (Min a)
--   -- Defined in ‘Data.Semigroup’
-- instance Monoid m => Monoid (WrappedMonoid m)
--   -- Defined in ‘Data.Semigroup’
-- instance (Applicative f, Monoid a) => Monoid (Ap f a)
--   -- Defined in ‘Data.Monoid’
-- instance Monoid (Data.Monoid.First a) -- Defined in ‘Data.Monoid’
-- instance Monoid (Data.Monoid.Last a) -- Defined in ‘Data.Monoid’
-- instance Monoid All
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance GHC.Base.Alternative f => Monoid (Alt f a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid Any
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid a => Monoid (Dual a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Monoid (Endo a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Monoid (Product a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’
-- instance Num a => Monoid (Sum a)
--   -- Defined in ‘base-4.18.1.0:Data.Semigroup.Internal’

-------------------------------------------------------------------------
-- # Exemples arithmétiques

-- >>> (Sum 4) <> (Sum 8)
-- Sum {getSum = 12}

-- >>> (Product 4) <> (Product 8)
-- Product {getProduct = 32}

-- >>> (Max 4) <> (Max 8)
-- Max {getMax = 8}

-- >>> :t Max 4
-- Max 4 :: Num a => Max a

-- Remarque : dans Semigroupe, cela fonctionne
-- >>> (Max 4 :: Max Integer ) <> (Max 8)
-- Max {getMax = 8}

-- ... Mais pas de Monoide pour les entiers
-- >>> (mempty :: Max Integer) <> (Max 8)
-- <interactive>:107:3-8: error: [GHC-39999]
--     • No instance for ‘Bounded Integer’ arising from a use of ‘mempty’
--     • In the first argument of ‘(<>)’, namely ‘(mempty :: Max Integer)’
--       In the expression: (mempty :: Max Integer) <> (Max 8)
--       In an equation for ‘it’: it = (mempty :: Max Integer) <> (Max 8)

-- >>> (Min 4) <> (Min 8
-- Min {getMin = 4}



-------------------------------------------------------------------------
-- # Autres exemples de semi-groupes
-- # (non-monoïdes)

-- >>> :info Semigroup

-- ## Remarque : les semi-groupes sans élément neutre sont assez rares
-- (cas particulier, les listes non-vides, cf. TD)

-------------------------------------------------------------------------
-- # Application des monoïdes : les foldables

-- Considérons la typeclasse suivante :

-- >>> :info Foldable
-- type Foldable :: (* -> *) -> Constraint
-- class Foldable t where
--   F.fold :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   F.foldMap' :: Monoid m => (a -> m) -> t a -> m
--   foldr :: (a -> b -> b) -> b -> t a -> b
--   F.foldr' :: (a -> b -> b) -> b -> t a -> b
--   foldl :: (b -> a -> b) -> b -> t a -> b
--   F.foldl' :: (b -> a -> b) -> b -> t a -> b
--   foldr1 :: (a -> a -> a) -> t a -> a
--   foldl1 :: (a -> a -> a) -> t a -> a
--   F.toList :: t a -> [a]
--   null :: t a -> Bool
--   length :: t a -> Int
--   elem :: Eq a => a -> t a -> Bool
--   maximum :: Ord a => t a -> a
--   minimum :: Ord a => t a -> a
--   sum :: Num a => t a -> a
--   product :: Num a => t a -> a
--   {-# MINIMAL foldMap | foldr #-}
--   	-- Defined in ‘Data.Foldable’
-- instance [safe] Foldable List
--   -- Defined at /tmp/dantecK1ovJ.hs:811:10
-- instance Foldable (Arg a) -- Defined in ‘Data.Semigroup’
-- instance Foldable Data.Semigroup.First
--   -- Defined in ‘Data.Semigroup’
-- instance Foldable Data.Semigroup.Last
--   -- Defined in ‘Data.Semigroup’
-- instance Foldable Max -- Defined in ‘Data.Semigroup’
-- instance Foldable Min -- Defined in ‘Data.Semigroup’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
-- instance Foldable f => Foldable (Alt f)
--   -- Defined in ‘Data.Foldable’
-- instance Foldable f => Foldable (Ap f)
--   -- Defined in ‘Data.Foldable’
-- instance Foldable Dual -- Defined in ‘Data.Foldable’
-- instance Foldable (Either a) -- Defined in ‘Data.Foldable’
-- instance Foldable Data.Monoid.First -- Defined in ‘Data.Foldable’
-- instance Foldable Data.Monoid.Last -- Defined in ‘Data.Foldable’
-- instance Foldable [] -- Defined in ‘Data.Foldable’
-- instance Foldable Maybe -- Defined in ‘Data.Foldable’
-- instance Foldable Product -- Defined in ‘Data.Foldable’
-- instance Foldable Solo -- Defined in ‘Data.Foldable’
-- instance Foldable Sum -- Defined in ‘Data.Foldable’

-- ### Remarquer en particulier la méthode foldMap ...

-- Intuitivement:

-- ### foldMap <inject> <struct>

-- avec :
--  <inject> :: a -> m     (m est un monoïde)
-- et <struct> :: t a      (t est un foldable)

-- Injecte tous les éléments de la structure <struct>
-- dans le monoide, en commençant par l'élément neutre mempty.

-------------------------------------------------------------------------
-- # Foldable : exemple des listes

listFoldMap :: Monoid m => (a -> m) -> List a -> m
listFoldMap _ Nil = mempty
listFoldMap inject (Cons x xs) = (inject x) <> listFoldMap inject xs

instance Foldable List where
  foldMap = listFoldMap

-- >>> :t Sum    -- ici, m =  Sum a   (pour tout type a "sommable")
-- Sum :: a -> Sum a

-- >>> foldMap Sum (Cons 2 (Cons 3 (Cons 4 Nil)))
-- Sum {getSum = 9}

-- >>> :t Product
-- Product :: a -> Product a

-- >>> foldMap Product (Cons 2 (Cons 3 (Cons 4 Nil)))
-- Product {getProduct = 24}

-- >>> :t (\x -> [x])
-- (\x -> [x]) :: a -> [a]

-- >>> foldMap (\x -> [x]) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- [1,2,3]


-- ## Remarque
-- - avec foldr, le concept d'accumulation est explicite
-- - avec foldMap, l'accumulation se fait implicitement grâce au monoïde

-------------------------------------------------------------------------

-- # Conclusion

-- ### Les structures algébriques comme Functor, Semigroup et Monoid
-- ### s'apparentent à des design patterns fonctionnels

-- - avec de nombreux cas pratiques

-- - et des solutions uniformes

-- - Issus de la théorie des catégories avec un maître-mot : la composition 
--   (compositionnalité ...)

-- ### ... en plus, en Haskell (et notamment PureScript, lean4) ... :

-- - Grâce notamment aux typeclass, les patterns peuvent être en partie formalisés
--   dans le système de type avec l'avantage de la vérification statique
--   (pour les lois il faut penser à quickcheck)

-- - programmation générique : les structures de bases sont exploitées dans des structures
--   plus complexes (ex. Foldable)


-- ### Suite du programme (cours suivants) :

-- - les foncteurs applicatifs et les structures traversables

-- - les monades  <-- point culminant du cours !

-- - les monad transformers