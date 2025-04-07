-- Pour typer explicitement les méthodes
{-# LANGUAGE InstanceSigs #-}

module PAF2023_Cours8 where

-- La bibliothèque standard pour les monades
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

--
import System.IO

-- ====================================================================
-- # PAF 8 : Structures algébriques (3/3)

-- #         Les Monades
-- ### (a.k.a. «le cours le plus important de votre vie»)

-- ## - Préliminaires
-- ### (a.k.a. «vous auriez pu inventer les monades vous-même»)

-- ## - La typeclass Monad et la notation do

-- ## - Instanciation de la monade (My)Maybe

-- ## - Les lois monadiques et  «le fish»

-- ### ------------------------------------------------
-- ### Copyright © 2023, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

-- ====================================================================
-- # La programmation monadiques

-- S'il ne fallait retenir qu'un seul  «design pattern» de la
-- programmation fonctionnelle (relativement) avancée, ce serait
-- sans doute:

-- # >>>>>>>> Les Monades <<<<<<<<<<
-- (limonade ?)

-- Les monades sont omni-présentes en programmation fonctionnelle
-- en général et en Haskell en particulier.

-- D'un point de vue algébrique, les monades correspondent
-- à un raffinement de la notion de contexte que nous avons abordé
-- précédemment, avec :

-- ### • au niveau le plus général, le contexte fonctoriel
--   (et la typeclass Functor)

-- ### • commme premier raffinement, le contexte des foncteurs applicatifs
--   (et la typeclass Applicative qui «hérite» de Functor)

-- ### • commme second raffinement, le contexte des monade
--   (et la typeclass Monad qui «hérite» de Applicative)

-- ... mais avant d'aborder la typeclass Monad, nous vous proposons
-- en quelque sorte de «réinventer» le concept...
-- (avec l'objectif de quelque peu le démystifier)

-- ====================================================================
-- # Préliminaires
-- ## Vous auriez pu (ré)inventer le concept de Monad

-- De façon très schématique, les monades offrent une réponse à la question :

-- ## Q: Comment combiner (composer, chaîner) des calculs contextuels ?

-- ## Exemple 1 : composer des calculs optionnels

newtype StudentId = StudentId String
  deriving (Show, Eq, Ord)

newtype Email = Email String
  deriving (Show, Eq)

emailsDB :: Map StudentId Email
emailsDB = Map.fromList [ (StudentId "321001", Email "pb@toto.net")
                          , (StudentId "321042", Email "en@tuti.fr")]


loginsDB :: Map String StudentId
loginsDB = Map.fromList [ ("pabe", StudentId "321001")
                      , ("enou", StudentId "321042") ]

-- ## L'opération qui nous intéresse est la suivante :
-- (ici implémentée de façon adhoc)

emailFromLogin :: String -> Maybe Email
emailFromLogin login =
  case Map.lookup login loginsDB of
    Nothing -> Nothing
    Just id -> Map.lookup id emailsDB

-- >>> emailFromLogin "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLogin "boule"
-- Nothing

-- ====================================================================
-- # Combiner des calculs contextuels (V1)

-- La fonction précédente ne pose pas de problème en soi, mais elle
-- «mélange» deux choses que l'on aimerait pouvoir séparer :

-- (1) la recherche d'un identifiant (id) à partir d'un login
-- (2) la recherche d'un email à partir d'un id

-- ### Nous pouvons décomposer les deux opérations:

fetchId :: String -> Maybe StudentId
fetchId login = Map.lookup login loginsDB

fetchEmail :: StudentId -> Maybe Email
fetchEmail id = Map.lookup id emailsDB

-- ### Notre objectif, alors, est de recombiner ces deux opérations
-- ### pour retrouver l'opération emailFromlogin

-- Cependant, la composition «de base» est inopérante car hors-contexte

-- ### On peut cependant définir une fonction qui effectue exactement
-- ### ce qui permet de combiner les deux opérations :

mcombine1 :: Maybe StudentId -> (StudentId -> Maybe Email) -> Maybe Email
mcombine1 (Just v) f = f v
mcombine1 Nothing _ = Nothing


-- ### On a par exemple :

-- >>> (fetchId "pabe") `mcombine1` fetchEmail
-- Just (Email "pb@toto.net")

-- >>> (fetchId "boule") `mcombine1` fetchEmail
-- Nothing

-- ### On remarque cependant que la fonction `mcombine1` se généralise
-- ### très naturellement à tout type de calcul optionnel.

-- ====================================================================
-- # Combiner des calculs contextuels (V2)

mcombine (Just v) f = f v
mcombine Nothing _ = Nothing

-- >>> :t mcombine
-- mcombine :: Maybe a -> (a -> Maybe b) -> Maybe b


-- ### et on a ainsi :

-- >>> (fetchId "pabe") `mcombine` fetchEmail
-- Just (Email "pb@toto.net")

-- >>> (fetchId "boule") `mcombine` fetchEmail
-- Nothing

-- ### On en déduit donc :

emailFromLoginV2 :: String -> Maybe Email
emailFromLoginV2 login = (fetchId login) `mcombine` fetchEmail

-- >>> emailFromLoginV2 "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLoginV2 "boule"
-- Nothing

-- ====================================================================
-- # Exemple 2 : calculs non-déterministes

-- ### On considère maintenant les fonctions suivantes :

finRange :: Int -> Int -> [Int]
finRange m n | m < n = m : finRange (m+1) n
             | otherwise = []

-- >>> finRange 1 8
-- [1,2,3,4,5,6,7]

sqList :: Num a => a -> [a]
sqList x = [(x * x)]

-- >>> sqList 8
-- [64]

-- ## Exercice :
-- ### Définir la fonction `lcombine` générique telle que :

-- >>> (finRange 1 8) `lcombine` sqList
-- [1,4,9,16,25,36,49]

-- ### Réponse :

-- lcombine :: [a] -> (a -> [b]) -> [b]
lcombine [] _ = []
lcombine (x:xs) f = (f x) <> (lcombine xs f)

-- ### Autre exemple de composition :

-- >>> (finRange 1 5) `lcombine` (\x -> ['A', 'B'] `lcombine` (\y -> [(x, y)]))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k []
-- [] :: * -> *

-- ====================================================================
-- # Exemple 3 : les entrées/sorties (IO)

-- ### On rappelle la signature de quelques fonctions élémentaires d'entrées/sorties

-- >>> :t getLine
-- getLine :: IO String

-- >>> :t putStrLn
-- putStrLn :: String -> IO ()

-- ## Exercice :
-- ### Donner la signature de la fonction `iocombine1` la plus spécifique
-- ### possible et telle quel que la combinaison suivante soit bien typée :

echoV1 = getLine `iocombine1` putStrLn

iocombine1 :: IO String -> (String -> IO ()) -> IO ()
iocombine1 act f = do
  val <- act
  f val

-- ## Exercice :
-- ### Proposer une signature plus générale `iocombine` avec des types paramétrés
-- ### et le même comportement

iocombine :: IO a -> (a -> IO b) -> IO b
iocombine act f = do
  val <- act
  f val

echoV2 :: IO ()
echoV2 = getLine `iocombine` putStrLn

-- ====================================================================
-- # Le combinateur «universel» : bind

-- ### Comparons les signatures des quelques «combines» que nous
-- ### avons écrites :

-- >>> :t mcombine
-- mcombine :: Maybe a -> (a -> Maybe b) -> Maybe b

-- >>> :t lcombine
-- lcombine :: [a] -> (a -> [b]) -> [b]

-- >>> :t iocombine
-- iocombine :: IO a -> (a -> IO b) -> IO b

-- ### N'existerait-il pas un principe plus général ?
-- ### ==> cherchons dans Hoogle la signature suivante :
-- x a -> (a -> x b) -> x b

-- ### On trouve :

-- >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- >>> :info (>>=)
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   ...

-- ### Ce combinateur général (>>=) s'appelle «bind»
-- ### et il est défini comme méthode de la typeclass Monad
-- ### ... vérifions son comportement

-- ====================================================================
-- # 1) sur les calculs optionnels

-- ## Rappels :
-- ### mcombine :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ### (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- ### emailFromLoginV2 :: String -> Maybe Email
-- ### emailFromLoginV2 login = (fetchId login) `mcombine` fetchEmail

-- ### Ainsi :

emailFromLoginV3 :: String -> Maybe Email
emailFromLoginV3 login = (fetchId login) >>= fetchEmail

-- >>> emailFromLoginV3 "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLoginV3 "boule"
-- Nothing

-- ====================================================================
-- # 2) sur les calculs non-déterministes

-- ## Rappels :

-- >>> :t lcombine
-- lcombine :: [a] -> (a -> [b]) -> [b]

-- >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- ### Ainsi :

-- >>> (finRange 1 8) `lcombine` sqList
-- [1,4,9,16,25,36,49]

-- >>> (finRange 1 8) >>= sqList
-- [1,4,9,16,25,36,49]

-- >>> (finRange 1 5) `lcombine` (\x -> ['A', 'B'] `lcombine` (\y -> [(x, y)]))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- >>> (finRange 1 5) >>= (\x -> ['A', 'B'] >>= (\y -> [(x, y)]))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- ====================================================================
-- # 3) sur les entrées/sorties

-- ## Rappels :

-- >>> :t iocombine
-- iocombine :: IO a -> (a -> IO b) -> IO b

-- >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- ### Ainsi :

-- echoV2 :: IO ()
-- echoV2 = getLine `iocombine` putStrLn

echoV3 :: IO ()
echoV3 = getLine >>= putStrLn

-- ====================================================================
-- # La typeclass Monad

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
-- instance Monad (Either e) -- Defined in ‘Data.Either’
-- instance Monad []
-- instance Monad Maybe
-- instance Monad IO
-- instance Monad ((->) r)
-- instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c)
-- instance (Monoid a, Monoid b) => Monad ((,,) a b)
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’

-- ### Un constructeur de type `m` de kind :: * -> *
-- ### est instance de Monad et appellé un «contexte monadique» si il est :

-- ### 1) instance de Functor et implémente donc (au minimum) :
-- fmap :: (a -> b) -> m a -> m b

-- ### 2) instance de Applicative et implémente donc (en plus) :
-- pure :: a -> m a
-- (<*>) :: m (a -> b) -> m a -> m b

-- ### 3) instance de Monad et implémente donc (en plus) :
-- (>>=) :: m a -> (a -> m b) -> m b

-- ### ==> on a vu que Maybe, [] et IO  sont des contextes monadiques
-- ### Il y a également le reader (-> r) pour tout r :: *
-- ### et de nombreux autres exemples dans des bibliothèques tierces

-- ====================================================================
-- # La notation do ...

-- L'opérateur (>>=) introduit une forme de séquentialité
-- dans les calculs, en particulier pour IO.

-- Haskell propose une notation qui reflète plus explicitement
-- ce caractère séquentiel : la notation do ...
-- (que nous avons déjà beaucoup utilisé dans IO)

-- ## Considérons l'exemple suivant, dans IO :

fact :: Integer -> Integer
fact n = aux n 1
  where aux n acc | n > 0 = aux (n-1) (n*acc)
                  | otherwise = acc

computeFact :: IO ()
computeFact = do
  putStr "n = "
  hFlush stdout
  s <- getLine
  let n = read s :: Integer  -- version sûre, cf. `reads`
  putStr $ "La factorielle de " <> (show n) <> " est: "
  putStrLn $ show $ fact n

-- ### Ce programme semble «naturel» (au moins si on a quelques années d'«impératif» ...)
-- ### Pourtant, le compilateur réécrit ce programme de la façon suivante :

computeFact' :: IO ()
computeFact' =
  putStr "n = "
  >> hFlush stdout
  >> getLine
  >>= (\s -> let n = read s :: Integer
             in (putStr ("La factorielle de " <> (show n) <> " est: ")
                 >> (putStrLn (show (fact n)))))

-- ### Remarque : (>>) est la version «non-dépendante» de (>>=)

-- >>> :t (>>)

-- ### -- remarque : on n'utilise pas le `a`

-- On peut dire que la notation do ... améliore quelque peu la lisibilité
-- des programmes (au moins dans IO).

-- ====================================================================
-- # La notation do ... en dehors de IO

-- ### Exemple avec Maybe :

-- >>> (Just 42) >>= (\x -> pure (x+1)) >>= (\y -> pure (even y))
-- Just False

-- >>> do { x <- Just 42 ; y <- pure (x+1) ; pure (even y) }
-- Just False

-- ### Remarque : on préfère en général écrire les do ... sur plusieurs
-- ### lignes plutôt que d'utiliser les accolades `{` et `}` et le point-virgule
-- (sauf pour les one-liners)

-- ### Ceci nous permet d'écrire (encore) une autre version de notre fonction de départ :

-- emailFromLoginV3 :: String -> Maybe Email
-- emailFromLoginV3 login = (fetchId login) >>= fetchEmail

emailFromLoginV4 :: String -> Maybe Email
emailFromLoginV4 login = do
  id <- fetchId login
  fetchEmail id

-- >>> emailFromLoginV4 "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLoginV4 "boule"
-- Nothing

-- ### Pour cet exemple, on ne gagne pas forcément en lisibilité,
-- ### mais on gagne beaucoup à transformer des :

-- case maybe1 y of
--   Nothing -> Nothing
--   Just x1 -> case maybe1 x1 of
--                 Nothing -> Nothing
--                 Just x2 -> case ...

-- ### en :

-- do
--   x1 <- maybe1 y
--   x2 <- maybe2 x1
--   ...

-- ====================================================================
-- # La notation do : règles de transformation

-- ### Les règles de transformations utilisées
-- ### pour transformer les expressions avec do ...
-- ### en combinaisons avec (>>=) etc.
-- ### sont les suivantes :

-- do expr → expr

-- do { expr ; nexts } → expr >> do { nexts }

-- do { v <- expr ; nexts } → expr >>= \v -> do { nexts }

-- do { let x = expr ; nexts } → let x = expr in do { nexts }

-- ### Remarque :
--   il s'agit d'une version quelque peu simplifiée de la transformation

-- ====================================================================
-- # Instanciation de Monad : exemple de Maybe

-- Nous continuons notre exemple des cours précédents

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq)

-- ### -- Cours 6 (Functor)
instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing = MyNothing
  fmap g (MyJust v) = MyJust (g v)

-- ### -- Cours 7 (Applicative)
instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = MyJust

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (MyJust g) <*> (MyJust x) = MyJust (g x)
  _ <*> _ = MyNothing

-- ====================================================================
-- # Le bind pour Maybe

-- ### On reprend notre «combine» :

-- mcombine (Just v) f = f v
-- mcombine Nothing _ = Nothing

instance Monad MyMaybe where
  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (MyJust v) >>= f = f v
  MyNothing >>= _ = MyNothing

-- ### Testons notre implémentation

-- >>> (MyJust 42) >>= (\x -> MyJust (x+1)) >>= (\y -> MyJust (even y))
-- MyJust False

-- >>> (MyJust 42) >>= (\x -> MyNothing) >>= (\y -> MyJust (even y))
-- MyNothing

-- On a bien sûr la même implémentation dans le prélude :

-- >>> (Just 42) >>= (\x -> Just (x+1)) >>= (\y -> Just (even y))
-- Just False

-- >>> (Just 42) >>= (\x -> Nothing) >>= (\y -> Just (even y))
-- Nothing

-- ### ... mais quid des lois ?

-- ====================================================================
-- # Les lois monadiques

-- Dans cette section, nous allons présenter les lois monadiques
-- mais contrairement aux foncteurs applicatifs, nous allons
-- expliquer le pourquoi et le comment de ces lois
-- ==> cette partie du cours est donc un peu plus «théorique».

-- On peut donner les lois monadiques en se basant sur pure/return
-- et sur bind (>>=) mais il existe une caractérisation beaucoup
-- plus élégante, issue comme pour Functor de la théorie des catégories.

-- ### Si on reprend l'exemple précédent :

-- emailFromLoginV3 :: String -> Maybe Email
-- emailFromLoginV3 login = (fetchId login) >>= fetchEmail

-- ### on peut dire que le bind (>>=) correspond plus à une sorte
-- ### de chaînage que de «composition».

-- Alternativement, on peut s'intéresser à un principe de «composition monadique»
-- inspirée de la composition de fonctions hors-contextes :

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- >>> :t fetchId
-- fetchId :: String -> Maybe StudentId

-- >>> :t fetchEmail
-- fetchEmail :: StudentId -> Maybe Email

-- ### L'équivalent monadique aurait la signature suivante :

-- (b -> m c) -> (a -> m b) -> (a -> m c)

-- (avec a=String, b=StudentId et c=Email)


-- ### On compose donc des fonction dont la signature est :  x -> m y
-- ### (pour un contexte monadique `m`)

-- ### Hoogle nous propose un candidat pour cette signature :

-- >>> :t (<=<)
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- ### On aussi une variante dont l'utilisation est un peu plus intuitive :

-- >>> :t (>=>)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- ### Ces opérateurs se nomment respectivement le «fish» gauche et le «fish» droite
-- ### (ou right-to-left  et  left-to-right fish)

-- ### Notre composition d'opérations peut donc encore une fois être réécrite,
-- ### de la façon suivante :

emailFromLoginV5 :: String -> Maybe Email
emailFromLoginV5 = fetchId >=> fetchEmail

-- >>> emailFromLoginV5 "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLoginV5 "boule"
-- Nothing

-- ### Il s'agit de la version sans doute la plus élégante de
-- ### cette fonction, car uniquement basée sur un principe
-- ### assez universel de «composition contextuelle».

-- ====================================================================
-- # Un dernier (?) détour par les catégories

-- ## Rappel 1 : les catégories

-- [[[(image :type imagemagick :file "whatisacategory.png" :width 900)]]]

-- ====================================================================
-- # Un dernier (?) détour par les catégories

-- ## Rappel 1 : les catégories

-- [[[(image :type imagemagick :file "categorylaws.png" :width 900)]]]


-- ====================================================================
-- # Catégories de Kleisli

-- ### Nous nous intéressons aux lois monadiques, et pour cela nous
-- ### allons considérer les catégories dites de Kleisli (en version Hask) avec :

-- ### • un contexte monadique `m` fixé, et

-- ### • les objets sont des types de Hask, considérés :
-- ###   hors-contexte pour le domaine des morphismes :
-- a, b, c, ...
-- ###   dans le contexte `m` pour le codomaine :
-- m a, m b, m c, ...

-- ### • les morphismes (flèches) sont des fonctions de la forme: 
-- f :: a -> m b, g :: b -> m c, ...

-- ### On a alors :

-- ## des identités

-- avec une signature de la forme :  a -> m a
-- et c'est donc pure/return

-- >>> :t pure
-- pure :: Applicative f => a -> f a

-- >>> :t return
-- return :: Monad m => a -> m a

-- ## des compositions

-- avec une signature de la forme :
-- et c'est donc le «fish» (celui que l'on veut)

-- >>> :t (>=>)
-- (>=>) :: Monad m => (x -> m y) -> (y -> m z) -> x -> m z

-- ====================================================================
-- # Les lois catégoriques (de Kleisli)

-- Nous rappelons les lois des catégories

-- ## Lois d'identité

-- ### Identité à gauche :  id . f = f

-- pour f :: a -> m b, on doit donc avoir :  return >=> f = f

-- ### Identité à droite :  f . id = f

-- pour f :: a -> m b, on doit donc avoir :  f >=> return = f

-- ## Loi d'associativité : f . (g . h) = (f . g) . h

-- pour f :: a -> m b, g :: b -> m c et h :: c -> m d :
-- f >=> (g >=> h) = (f >=> g) >=> h

-- ### Ce sont les 3 lois que les monades doivent respecter.
-- ### En TD, nous les réécrirons avec pure/return et (>>=) mais on constate à nouveau
-- ### notre principale source d'informations : la théorie des catégories.


