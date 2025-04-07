-- -- Quelques extensions pour cette semaine ...

-- -- Typage explicite des méthodes d'instances
{-# LANGUAGE InstanceSigs #-}
-- -- Instantiation plus permissive
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cours5_script where

-- -- Bonne pratique : Text plutôt que String
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Read

-- -- notre implication habituelle
infixr 2 ==>
(==>) :: Bool -> Bool -> Bool
(==>) a b = (not a) || b


-- ========================================================================

-- # PAF 5 : Polymorphism ad-hoc
-- #         et Typeclasses

-- ## - Concepts fondamentaux

-- ## - Typeclasses de base: Show, Eq, Ord, etc.

-- ## - Définition de Typeclasses

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)



-- ========================================================================

-- # Polymorphisme ad-hoc : motivations

-- ## Point de départ : le polymorphisme paramétrique

-- ### Exemple : la fonction identité

identity :: a -> a
identity x = x

-- >>> identity 42
-- 42


-- >>> identity True
-- True

-- ========================================================================

-- # Polymorphisme ad-hoc : motivations

-- ## Point de départ : le polymorphisme paramétrique

-- ### Exemple : la fonction identité

-- >>> :t identity
-- identity :: a -> a


-- ## Type de identity

-- Pour tout type a, la fonction identity prend un a et retourne un a

-- ### ∀ a :: *, identity :: a -> a

-- ========================================================================

-- # Polymorphisme ad-hoc : motivations

-- ## Point de départ : le polymorphisme paramétrique

-- ### Exemple : la fonction identité

-- >>> :t identity
-- identity :: a -> a

-- ## Type de identity

-- Pour tout type a, la fonction identity prend un a et retourne un a

-- ### ∀ a :: *, identity :: a -> a

-- ## Avantage
-- un même comportement pour n'importe quel type `a`

-- ========================================================================

-- # Polymorphisme ad-hoc : motivations

-- ## Point de départ : le polymorphisme paramétrique

-- ### Exemple : la fonction identité

-- >>> :t identity
-- identity :: a -> a

-- ## Type de identity

-- Pour tout type a, la fonction identity prend un a et retourne un a

-- ### ∀ a :: *, identity :: a -> a

-- ## Avantage
-- un même comportement pour n'importe quel type `a`

-- ## Inconvénient
-- ce comportement doit être indépendant de ce type `a`

-- ========================================================================

-- # Polymorphisme ad-hoc : motivations

-- ## Exemple : calculs arithmétiques

periInt :: Int -> Int -> Int
periInt x y = 2 * (x + y)

periFloat :: Float -> Float -> Float
periFloat x y = 2 * (x + y)

-- ==> Un même comportement pour des types différents

-- ### Haskell accepte ce «comportement générique»

peri x y = 2 * (x + y)

-- ### mais avec quel type ?

-- >>> :t peri
-- peri :: Num a => a -> a -> a

-- Pour tout type `a` tel que la contrainte (Num a)
-- est satisfaite, peri :: a -> a -> a

-- ========================================================================
  
-- # La contrainte (Num a)

-- Un type a satisfait (Num a) si il est instance de la typeclass Num

-- >>> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--   	-- Defined in ‘GHC.Num’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’

-- >>> peri (2 :: Integer) 5
-- 14

-- >>> :t 3.2
-- 3.2 :: Fractional a => a

-- >>> :t peri 24 3.2
-- peri 24 3.2 :: Fractional a => a

-- ### Mais attention à bien respecter le paramètre de type

-- >>> peri (2.4 :: Float) (3 :: Int)


-- ========================================================================

-- # Terminologie

-- ## Typeclass
-- La définition d'une contrainte est appelée une typeclass

-- ## Méthode
-- Chaque signature à implémenter se nomme une méthode

-- ## Instance
-- Un type concret qui satisfait la constrainte est dit instance de la typeclass

-- # ⚠ ⚠ ⚠
-- Cette terminologie à connotation «OO» est trompeuse
-- ==> le concept OO le plus proche (mais distinct) est celui d'interface

-- ========================================================================

-- # Typeclasses de base

-- Quelques typeclasses de base fréquemment instanciées

-- ## Représentation textuelle avec Show

-- ## Egalité avec Eq

-- ## Ordre total avec Ord

-- ## Arithmétique avec Num (etc.)
-- ==> cf. TME 5


-- ========================================================================

-- # Représentation textuelle : Show

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}
--   	-- Defined in ‘GHC.Show’
-- instance [safe] Show Figure
--   -- Defined at /tmp/danteGH3AVp.hs:254:13
-- instance [safe] Show Figure'
--   -- Defined at /tmp/danteGH3AVp.hs:286:10
-- instance [safe] Show Fraction
--   -- Defined at /tmp/danteGH3AVp.hs:415:10
-- instance [safe] (Show a, Show b) => Show (Pair a b)
--   -- Defined at /tmp/danteGH3AVp.hs:511:13
-- instance [safe] (Show a, Show b) => Show (Pair' a b)
--   -- Defined at /tmp/danteGH3AVp.hs:551:13
-- instance [safe] (Show a, Show b) => Show (ProdPair a b)
--   -- Defined at /tmp/danteGH3AVp.hs:584:13
-- instance Show Double -- Defined in ‘GHC.Float’
-- instance Show Float -- Defined in ‘GHC.Float’
-- instance Show Text -- Defined in ‘text-2.0.2:Data.Text.Show’
-- instance Show () -- Defined in ‘GHC.Show’
-- instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c) => Show (a, b, c)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e) =>
--          Show (a, b, c, d, e)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f) =>
--          Show (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f,
--           Show g) =>
--          Show (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h) =>
--          Show (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i) =>
--          Show (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j) =>
--          Show (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Show’
-- instance Show Bool -- Defined in ‘GHC.Show’
-- instance Show Char -- Defined in ‘GHC.Show’
-- instance Show Int -- Defined in ‘GHC.Show’
-- instance Show Integer -- Defined in ‘GHC.Show’
-- instance Show a => Show [a] -- Defined in ‘GHC.Show’
-- instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
-- instance Show Ordering -- Defined in ‘GHC.Show’
-- instance Show ghc-prim-0.10.0:GHC.Types.RuntimeRep
--   -- Defined in ‘GHC.Show’
-- instance Show a => Show (Solo a) -- Defined in ‘GHC.Show’
-- instance Show Word -- Defined in ‘GHC.Show’
-- instance (Show a, Show b) => Show (Either a b)
--   -- Defined in ‘Data.Either’

-- La méthode principale : show

-- >>> :t show
-- show :: Show a => a -> String

-- >>> show 42
-- "42"

-- >>> show "hello"
-- "\"hello\""

-- ========================================================================

-- # Dérivation : Instanciation automatique

-- Certaines typeclasses sont dites dérivables
-- ==> génération automatique des instances

-- Dans base, c'est notamment le cas de Show

-- ## Exemple : figures géométriques

data Figure =
  Cercle Float -- rayon
  | Triangle Float Float Float -- côtés
  | Rectangle Float Float -- côtés
  | Segment Float -- longueur
  deriving (Show, Eq, Ord)

-- >>> show $ Segment 4.3
-- "Segment 4.3"

-- >>> show $ Rectangle 2.3 5
-- "Rectangle 2.3 5.0"

-- ========================================================================

-- # Instanciation manuelle

data Figure' =
  Cercle' Float -- rayon
  | Triangle' Float Float Float -- côtés
  | Rectangle' Float Float -- côtés
  | Segment' Float -- longueur

-- >>> show $ Segment' 4.3

-- ### 1) fonctions de conversion

showFigure' :: Figure' -> String
showFigure' (Cercle' r) = "Cercle(" <> (show r) <> ")"
showFigure' (Triangle' a b c) = "Triangle(" <> (show a) <> "," <> (show b) <> "," <> (show c) <> ")"
showFigure' (Rectangle' a b) = "Rectangle(" <> (show a) <> "," <> (show b) <> ")"
showFigure' (Segment' d) = "Segment(" <> (show d) <> ")"

-- >>> showFigure' $ Segment' 4.3
-- "Segment(4.3)"

-- >>> showFigure' $ Rectangle' 2.3 5
-- "Rectangle(2.3,5.0)"

-- ### 2) instanciation

instance Show Figure' where
  show = showFigure'

-- >>> show $ Segment' 4.3
-- "Segment(4.3)"

-- >>> show $ Rectangle' 2.3 5
-- "Rectangle(2.3,5.0)"

-- ## Remarques
-- 1) on aurait pu définir la méthode directement dans l'instance, mais on ne peut plus
-- tester en dehors de la typeclass donc pratique plutôt déconseillée
-- (au moins quand l'implémentation est non-triviale)

-- 2) Show est uniquement utile pour le débogage et/ou les interactions dans GHCi
-- donc dans 99,9% des cas il est préférable de dériver automatiquement

-- ========================================================================

-- # Egalité : Eq

-- >>> :info Eq

-- Implémentation de l'égalité (==) et l'inégalité (/=)

-- ### Instanciation automatique / dérivation = égalité structurelle
-- ### Instanciation manuelle =  équivalence «comportementale» (≅ sémantique)

-- ========================================================================

-- # Les lois de l'égalité

-- ## ⚠ les typeclasses sont (en général) associées à des «lois fondamentales»
-- ==> Lawful typeclass

-- Notamment :

-- ### *Reflexivité* : x == x <=> True

law_Eq_refl :: Eq a => a -> Bool
law_Eq_refl x = x == x

-- ### *Symétrie* : x == y <=> y == x

law_Eq_sym :: Eq a => a -> a -> Bool
law_Eq_sym x y = (x == y) == (y == x)


-- ### *Transitivité* : si x == y et y == z = True, alors x == z = True

law_Eq_trans :: Eq a => a -> a -> a -> Bool
law_Eq_trans x y z = (x == y) && (y == z) ==> x == z

-- ### *Congruence* : si x == y = True, alors f x == f y = True

law_Eq_cong :: (Eq a, Eq b) => a -> a -> (a -> b) -> Bool
law_Eq_cong x y f = x == y ==> f x == f y

-- ### *Inégalité* : x /= y <=> not (x == y)

law_Eq_neg :: Eq a => a -> a -> Bool
law_Eq_neg x y = (x /= y) == not (x == y)

-- ## Remarques

-- 1) ces lois nécessitent d'expliciter les contraintes d'égalité dans leur signature

-- 2) pour des raisons techniques, certaines instances ne respectent pas certains lois, notamment avec les flottants (type `Double`), mais dans la mesure du possible il est clairement préférable de les respecter. Dans le cas contraire, il faut précisément expliquer quand et comment ces lois sont contredites.

-- ========================================================================

-- # Eq : égalité structurelle

-- Si on veut exploiter l'égalité structurelle, on privilégie la dérivation

-- ## Exemple : les figures géométriques

{- data Figure =
     Cercle Float -- rayon
     | ... etc ... 
   deriving (Show, Eq, Ord) -}

-- >>> Rectangle 2.3 4 == Rectangle 2.3 (2 + 2)
-- True

-- >>> Rectangle 2.3 4 /= Rectangle 2.3 (2 + 2)
-- False

-- >>> Cercle 3.2 == Segment 3.2
-- False

-- >>> [[1], [2, 3], [1, 2, 3]] == [1] : (2 : 3 : []) : [1, 1+1, 1+2] : []
-- True

-- ## Question : pourquoi n'a-t-on pas de fonction de comparaison générique ?
-- ==> parce que certaines données sont intrinsèquement non-comparables

-- ### Exemple 1 : les fonctions

-- >>> (\x -> x + 1) == (\z -> z + 2 -1)
-- <interactive>:92:16-17: error: [GHC-39999]
--     • No instance for ‘Eq (Integer -> Integer)’
--         arising from a use of ‘==’
--         (maybe you haven't applied a function to enough arguments?)
--     • In the expression: (\ x -> x + 1) == (\ z -> z + 2 - 1)
--       In an equation for ‘it’: it = (\ x -> x + 1) == (\ z -> z + 2 - 1)

-- ### Exemple 2 : les structures infinies

data Stream a = Cons a (Stream a)

-- ### Question : une égalité structurelle pour (Stream a) ?

-- Approximation :

gen :: Integer ->  Stream Integer
gen n = Cons n $ gen $ n + 1  

nats1 = gen 1
nats2 = Cons 1 (gen 2)

streamEqUntil :: Eq a => Integer -> Stream a -> Stream a -> Bool
streamEqUntil 0 _ _ = True
streamEqUntil k (Cons e1 s1) (Cons e2 s2)
  | k > 0 = e1 == e2 && streamEqUntil (k - 1) s1 s2
  | otherwise = error "Negative"

-- >>> streamEqUntil 1000 nats1 nats2 
-- True

-- ========================================================================

-- # Eq : équivalence comportementale

-- Dans certains (rares) cas, on ne souhaite pas exploiter
-- l'égalité structurelle. On parle parfois d'équivalence comportementale.
-- (et plus techniquement il faudrait parler de quotients)

-- ## Exemple : les rationnels

data Fraction = Fraction Integer Integer

instance Show Fraction where
  show :: Fraction -> String  -- nécessite l'extension InstanceSigs
  show (Fraction x y) = (show x) <> "/" <> (show y)

-- >>> Fraction 2 3
-- C:\Users\Admin\AppData\Local\Temp\ext151B: withFile: resource busy (file is locked)

-- >>> Fraction 4 6
--- 4/6

-- ### Classes d'équivalences des rationnels

pgcd :: Integer -> Integer -> Integer
pgcd 0 b = b
pgcd a 0 = a
pgcd a b = pgcd c $ d `mod` c
  where d = max a b
        c = min a b

-- >>> pgcd 48 4
-- 4

-- >>> pgcd 57 21
-- 3

-- >>> pgcd 11 47
-- 1

-- >>> pgcd 11 121
-- 11

fracNorm :: Fraction -> Fraction
fracNorm (Fraction _ 0) = error "Dénominateur nul"
fracNorm (Fraction a b) =
  case pgcd a b of
    0 -> Fraction 0 b
    p -> Fraction (a `div` p) (b `div` p) 


-- >>> fracNorm $ Fraction 2 3
-- 2/3

-- >>> fracNorm $ Fraction 4 6
-- 2/3

fracEq :: Fraction -> Fraction -> Bool
fracEq f1 f2 = let Fraction a1 b1 = fracNorm f1
                   Fraction a2 b2 = fracNorm f2
               in a1 == a2 && b1 == b2

-- >>> fracEq (Fraction 2 3) (Fraction 2 3)
-- True

-- >>> fracEq (Fraction 2 3) (Fraction 4 6)
-- True

-- >>> fracEq (Fraction 2 3) (Fraction 8 6)
-- False

   
instance Eq Fraction where
  (==) = fracEq


-- >>> Fraction 2 3 == Fraction 2 3
-- True

-- >>> Fraction 2 3 == Fraction 4 6
-- True

-- >>> Fraction 2 3 == Fraction 8 6
-- False


-- ### Remarque : les rationnels de base sont définis dans Data.Ratio

-- ========================================================================

-- # Contraintes explicites

-- Il y a principalement deux situations qui nécessitent de spécifier explicitement
-- les contraintes

-- ## 1) les contraintes sur la signature des fonctions

-- ## 2) la propagation des contraintes lors des instanciations
-- (également lorsque l'on définit de nouvelles contraintes, cf. suite du cours)

-- ========================================================================

-- # Contraintes sur les signature de fonctions

-- ## Exemples

-- >>> :t peri
-- peri :: Num a => a -> a -> a

-- >>> :t law_Eq_trans
-- law_Eq_trans :: Eq a => a -> a -> a -> Bool

-- ========================================================================

-- # Propagation des contraintes

-- Pour instancier des types polymorphes, il est souvent
-- nécessaire d'imposer des contraintes sur les paramètres de type.

-- ## Exemple : égalité structurelle pour les paires

data Pair a b = Pair a b
  deriving (Show)   -- pas de Eq, Ord pour l'exemple

-- instance Eq (Pair a b) where
--   -- (==) :: Pair a b -> Pair a b -> Bool
--   (Pair x y) == (Pair x' y') = x == x' && y == y'
-- ### error
-- ### • No instance for (Eq a) arising from a use of ‘==’
-- ###   Possible fix: add (Eq a) to the context of the instance declaration
-- ###   • In the first argument of ‘(&&)’, namely ‘x == x'’
-- ###     In the expression: x == x' && y == y'
-- ###     In an equation for ‘==’:
-- ###       (Pair x y) == (Pair x' y') = x == x' && y == y'  

-- ===> il faut imposer aux types a et b la contrainte Eq

instance (Eq a, Eq b) => Eq (Pair a b) where
  (==) :: Pair a b -> Pair a b -> Bool
  (Pair x y) == (Pair x' y') = x == x' && y == y'

-- >>> Pair 2 6 == Pair (1+1) (3*2)
-- True

-- ========================================================================

-- # Ordre total : Ord

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}
--   	-- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance [safe] Ord Figure -- Defined at /tmp/danteGH3AVp.hs:340:23
-- instance [safe] (Ord a, Ord b) => Ord (Pair' a b)
--   -- Defined at /tmp/danteGH3AVp.hs:668:23
-- instance [safe] (Ord a, Ord b) => Ord (ProdPair a b)
--   -- Defined at /tmp/danteGH3AVp.hs:703:10
-- instance Ord Text -- Defined in ‘Data.Text’
-- instance Ord () -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b) => Ord (a, b)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) =>
--          Ord (a, b, c, d, e, f)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) =>
--          Ord (a, b, c, d, e, f, g)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
--           Ord h) =>
--          Ord (a, b, c, d, e, f, g, h)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i) =>
--          Ord (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j) =>
--          Ord (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Bool -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Char -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Double -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Float -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Int -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord a => Ord [a]
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Ordering -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord a => Ord (Solo a)
--   -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord Word -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Ord Integer -- Defined in ‘GHC.Num.Integer’
-- instance (Ord a, Ord b) => Ord (Either a b)
--   -- Defined in ‘Data.Either’

-- ## Important :  Ord a  nécessite  Eq a
-- ==>   class Eq a => Ord a ...

-- >>> :t (<=)
-- (<=) :: Ord a => a -> a -> Bool

-- >>> :t compare
-- compare :: Ord a => a -> a -> Ordering

-- >>> :info Ordering
-- type Ordering :: *
-- data Ordering = LT | EQ | GT
--   	-- Defined in ‘ghc-prim-0.10.0:GHC.Types’
-- instance Bounded Ordering -- Defined in ‘GHC.Enum’
-- instance Enum Ordering -- Defined in ‘GHC.Enum’
-- instance Ord Ordering -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Read Ordering -- Defined in ‘GHC.Read’
-- instance Eq Ordering -- Defined in ‘ghc-prim-0.10.0:GHC.Classes’
-- instance Monoid Ordering -- Defined in ‘GHC.Base’
-- instance Semigroup Ordering -- Defined in ‘GHC.Base’
-- instance Show Ordering -- Defined in ‘GHC.Show’

-- ========================================================================

-- # Ordre par défaut : lexicographique

data Pair' a b = Pair' a b
  deriving (Show, Eq, Ord) -- Eq nécessaire !

-- >>> Pair' 2 4 <= Pair' 3 2
-- True

-- >>> Pair' 2 4 <= Pair' 3 5
-- True

-- >>> Pair' 2 4 > Pair' 2 3
-- True

-- >>> Pair' 2 4 < Pair' 1 4
-- False

-- ### C'est le cas des paires du prélude

-- >>> (2, 4) <= (3, 2)
-- True

-- >>> (2, 4) <= (3, 5)
-- True

-- >>> (2, 4) > (2, 3)
-- True

-- >>> (2, 4) < (1, 2)
-- False

-- ========================================================================

-- # Instances ambigües

-- Puisque plusieurs ordres sont possibles :
-- - lequel choisir par défaut ?
-- - comment en définir une alternative ?

-- ## Par défaut : lexicographique par dérivation

-- ## Alternatives :  avec newtype (1 constructeur) ou data + instanciation manuelle

data ProdPair a b = ProdPair a b
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (ProdPair a b) where
  (ProdPair x y) <= (ProdPair x' y') = x <= x' && y <= y'

-- >>> (2, 3) <=  (3, 1)
-- True

-- >>> ProdPair 2 3 <= ProdPair 3 1
-- False

-- ========================================================================

-- # L'ordre et les lois

-- Les lois à respecter, pour `<=` sont les suivantes :

-- ## réflexivité

prop_Leq_refl :: Ord a => a -> Bool
prop_Leq_refl x = (x <= x)

-- ## transitivité

prop_Leq_trans :: Ord a => a -> a -> a -> Bool
prop_Leq_trans x y z = (x <= y) && (y <= z) ==> (x <= z)

-- ## antisymétrie

prop_Leq_anti :: Ord a => a -> a -> Bool
prop_Leq_anti x y = (x <= y) && (y <= x) ==> (x == y)


-- ========================================================================

-- # Définition de Typeclass

-- Dans certaines situations (plutôt exceptionnelles), il est intéressant
-- de pouvoir définir ses propres typeclasses

-- ### En pratique, c'est relativement peu courant:
--   - concept Haskell de niveau intermédiaire+/avancé
--   - il existe un certain nombre d'«antipatterns»

-- ## Typeclass = formalisation d'un design pattern
-- ### ... (plutôt que ≅ interface) ...

-- ## Exemple : représentation textuelle

-- On s'inspire de Show mais pour Text

class Printable a where
  toText :: a -> Text

-- ## Important :  Typeclasse ==> lois

law_Printable_injection :: (Printable a, Eq a) => a -> a -> Bool
law_Printable_injection x y = x /= y ==> toText x /= toText y

-- ========================================================================

-- # Printable : exemples d'instances

instance Printable Integer where
  toText = Text.pack . show

instance Printable String where
  toText = Text.pack

instance Printable Text where
  toText = id

-- >>> toText (42 :: Integer)
-- "42"

-- >>> toText "hello"
-- "hello"

-- >>> toText $ toText "hello"
-- "hello"


-- ========================================================================

-- # Type de retour polymorphe

-- Un capacité unique des Typeclasses est de permettre
-- de contraindre le type de retour des fonctions

-- ## Exemple : lecture générique

-- Voici le complémentaire de Printable :

class Readable a where
  fromText :: Text -> Maybe a

-- ### Remarque: l'opération est partielle

-- ## Exemple de loi pour Readable vs. Printable

law_Readable_inverse :: (Readable a, Printable a, Eq a) => a -> Bool
law_Readable_inverse x = case (fromText (toText x)) of
                           Nothing -> error "cannot read"
                           Just y -> y == x

-- ### Question subsidiare : pouvez-vous exprimer la loi complémentaire ?
-- ==> pour tout `s` (avec les bonnes contraintes),
-- on aurait `fromText (toText s)  == s` ?


-- ========================================================================

-- # Readable : quelques instances

-- un premier cas facile
instance Readable Text where
  fromText :: Text -> Maybe Text
  fromText = Just

-- Attention : si on n'indique pas un type explicite
-- pour la valeur de retour, Haskell va utiliser le
-- type le plus générique possible ...

-- >>> :t Nothing
-- Nothing :: Maybe a

-- >>> fromText (toText "hello") 
-- Nothing

-- >>> fromText (toText "hello")  :: Maybe Text
-- Just "hello"

-- ## IMPORTANT
-- ### Quand on veut exploiter le polymorphisme du type de retour (et non des arguments)
-- ### il faut souvent «expliquer ce que l'on veut»  (sinon le typage est ambigû).

-- >>> fromText $ toText "hello"
-- Nothing

-- >>> :t fromText $ toText "hello"
-- fromText $ toText "hello" :: Readable a => Maybe a


-- ## Un cas un peu moins trivial
instance Readable Integer where
  fromText :: Text -> Maybe Integer
  fromText s = case Read.decimal s  of  -- parsing d'un décimal dans Text.Read
                 Left _ -> Nothing
                 Right (n :: Integer, _) -> Just n

-- ### Remarque : l'annotation de type pour `n` est nécessaire.

-- >>> fromText (toText "42")  :: Maybe Integer
-- Just 42

-- ========================================================================

-- # Typeclasses vs. interfaces (Java) ?

-- Les Typeclasses et les interfaces OO (java) ont des points communs
--   - spécifications de signatures à implémenter
--   - méthodes, instances, héritage (vocabulaire)


-- Cependant, la comparaison s'arrête assez vite

-- ## Monde ouvert vs. Monde fermé
-- ==>  on ne peut pas implémenter une interface sur un type déja défini
--      (il faut expliciter le "implements" dans la définition)

-- ## polymorphisme d'inclusion ≠ polymorphisme ad-hoc
--   ==> dispatch dynamique sur un argument (interfaces)
--   vs. dispatch statique prenant en compte le type de tous les arguments et le type de retour (typeclasses)

-- ## higher-kinded polymorphism (cf. prochain cours / Functor)

-- ## (anti-)exercice :
--  Printable et (surtout) Readable avec des interfaces Java

-- ## Typeclasses ailleurs ?
-- Scala 2 (implicits), Scala 3 (using/given), Ocaml/n
ext? (modular implicits?)
-- Purescript, Idris, Agda, Coq, Isabelle, Lean4, etc.
