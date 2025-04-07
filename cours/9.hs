{-# LANGUAGE InstanceSigs #-}
-- Pour typer explicitement les méthodes
{-# LANGUAGE BangPatterns #-}
-- Pour les annotations ! de strictness

module PAF2023_Cours9 where

-- La bibliothèque standard pour les monades
import Control.Applicative
import Control.Monad

import System.IO

-- ================================================================================
-- # PAF 9 : Monad Transformers
--                    /~@@~\,:
--  _______ . _\_\___/\ __ /\___|_|_ . _______
-- / ____  |=|      \  <_+>  /      |=|  ____ \                                _____       _____
-- ~|    |\|=|======\\______//======|=|/|    |~                   .........   {     }     {     }
--  |_   |    \      |      |      /    |    |                   (>>\zzzzzz [======================]
--   \==-|     \     |  2D  |     /     |----|~~)                ( <<<\lllll_\\ _        _____    \\
--   |   |      |    |      |    |      |____/~/                _,`-,\<   __#:\\::_    __#:::_:__  \\
--   |   |       \____\____/____/      /    / /                /    . `--,#::::\\:::___#::::/__+_\ _\\
--   |   |         {----------}       /____/ /                /  _  .`-    `--,/_~~~~~~~~~~~~~~~~~~~~  -,_
--   |___|        /~~~~~~~~~~~~\     |_/~|_|/   ........\    :,// \ .         .  '--,____________   ______`-
--    \_/        [/~~~~~||~~~~~\]     /__|\     ......../     :: o |.         .  ___ \_____||____\+/     ||~
--    | |         |    ||||    |     (/|[[\)                  :;   ;-,_       . ,' _`,""""""""""""""""""""""
--   -[_]        |     |  |     |                             \ \_/ _ :`-,_   . ; / \\ =====================
--               |_____|  |_____|                              \__/~ /     `-,.; ; o |\___[~~~]_ASCII__[~~~]
--               (_____)  (_____)                                 ~~~          ; :   ;~ ;  ~~~         ;~~~:
--               |     |  |     |                                               \ \_/ ~/               :::::
--               |     |  |     |                                                \_/~~/                 \:::
--               |/~~~\|  |/~~~\|                                                  ~~~                   ~~~
--               /|___|\  /|___|\
--              <_______><_______>

-- ## - Composer des Monades : une solution  «ad hoc»

-- ## - Composition de contextes : une solution générique ?

-- ## - Les Monad Transformers

-- ### ------------------------------------------------
-- ### Copyright © 2023, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

-- ================================================================================
-- # Composer des contextes

-- On a vu dans les précédents cours trois types de contextes :

-- ### • les contextes fonctoriels, instances de la typeclass Functor

-- ### • les contextes applicatifs, instances de Applicative

-- ### • les contextes monadiques, instances de Monad

-- En pratique, on travaille rarement dans un seul contexte, mais
-- plus généralement dans un «mélange» de contextes

-- Pour illustrer ce propos, nous allons considérer une application fictive
-- qui mélange trois types de contextes :

-- • l'application manipule des fonctions partielles, et se place dons dans le
-- contexte des calculs optionnels avec la monade  `Maybe`

-- • elle conserve une trace des calculs, et en première approche utilise
-- donc la monade `Writer`

-- • finalement, des entrées/sorties sont effectuées donc elle se place
-- dans le contexte de la monade IO

-- La question qui nous intéresse est la suivante :

-- ## Comment composer ces contextes (monadiques) ?

-- ================================================================================
-- # Composition de contexte : approche «ad hoc»

-- # Etape 1 : la monade Writer

-- ### Maybe et IO sont dans le prélude, mais pas Writer.
-- ### Voici une implémentation possible.

newtype Writer e a = Writer { runWriter :: (a, e) }
  deriving Show

instance Functor (Writer e) where
  fmap g (Writer (x, !y)) = Writer (g x, y)

instance Monoid e => Applicative (Writer e) where
  pure x = Writer (x, mempty)
  (Writer (g, !y1)) <*> (Writer (x, !y2)) = Writer (g x, y1 <> y2)

instance Monoid e => Monad (Writer e) where
  (>>=) :: Writer e a -> (a -> Writer e b) -> Writer e b
  (Writer (x, !y1)) >>= f = case f x of
                              Writer (x', !y2) -> Writer (x', y1 <> y2)


-- ## Remarques :
-- - On utilise des annotations de *strictness* pour forcer l'évaluation
--   des logs
-- - Dans une véritable application, on préférera utiliser une bibliothèque
--   spécialisée  (fast-logger, co-log ou autre)
--   et pour le débogage on peut utiliser Debug.Trace (trace)


-- ### En revanche, Writer reste utile pour accumuler de l'information
-- ### Une sorte d'accumulateur générique, donc.

-- -- On ajoute quelques définitions auxiliaires qui nous serons utiles.

tell :: [a] -> Writer [a] ()
tell xs = Writer ((), xs)

logg :: a -> Writer [a] ()
logg x = tell [x]

type Log = [String]

-- Et voici quelques fonctions qui vont nous
-- permettre de faire des calculs arithmétiques
-- dans le contexte de Writer
 
incrWithLog :: Integer -> Writer Log Integer
incrWithLog x = do
  logg $ "Increment de " <> (show x)
  return $ x + 1

-- >>> incrWithLog 10
-- Writer {runWriter = (11,["Increment de 10"])}

-- -- on peut aussi écrire :

-- >>> runWriter $ incrWithLog 10
-- (11,["Increment de 10"])

addWithLog :: Integer -> Integer -> Writer Log Integer
addWithLog x y = do
  logg $ "Addition de " <> (show x) <> " et " <> (show y)
  return $ x + y

-- >>> runWriter $ addWithLog 39 3
-- (42,["Addition de 39 et 3"])

-- -- en contexte fonctoriel

-- >>> runWriter $ fmap even (addWithLog 39 3)
-- (True,["Addition de 39 et 3"])

-- -- dans applicative

-- >>> runWriter $ (+) <$> (addWithLog 39 3) <*> (incrWithLog 10)
-- (53,["Addition de 39 et 3","Increment de 10"])

-- -- et finalement comme Monade
complexOp :: Integer -> Writer Log Integer
complexOp n = do
  x <- incrWithLog n
  y <- addWithLog n x
  divWithLog y $ x `div` 20

-- >>> runWriter $ complexOp 42
-- (42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])


-- ================================================================================
-- # Etape 2 : combiner Writer avec Maybe

-- ### Exemple de fonction partielle : la division

divWithLog :: Integer -> Integer -> Writer Log Integer
divWithLog x y = do
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `div` y

-- >>> runWriter $ divWithLog 42 2
-- (21,["Division de 42 par 2"])

-- >>> divWithLog 42 0
-- Writer {runWriter = (*** Exception: divide by zero

-- >>> complexOp 10
-- Writer {runWriter = (*** Exception: divide by zero

-- (en pratique, à chaque fois le programme s'arrête complètement !)


-- Voici une version sûre :

divSafe :: Integer -> Integer -> Maybe Integer
divSafe x y = do
  guard (y /= 0)
  return $ x `div` y

-- >>> divSafe 42 2  
-- Just 21

-- >>> divSafe 42 0
-- Nothing

divWithLogSafe :: Integer -> Integer -> Writer Log (Maybe Integer)
divWithLogSafe x y = do
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `divSafe` y

complexOpSafe :: Integer -> Writer Log (Maybe Integer)
complexOpSafe n = do
  x <- incrWithLog n
  y <- addWithLog n x
  divWithLogSafe y $ x `div` 20

-- >>> runWriter $ divWithLogSafe 39 3
-- (Just 13,["Division de 39 par 3"])

-- >>> runWriter $ divWithLogSafe 39 0
-- (Nothing,["Division de 39 par 0"])

-- >>> runWriter $ complexOpSafe 42
-- (Just 42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])

-- >>> runWriter $ complexOpSafe 10
-- (Nothing,["Increment de 10","Addition de 10 et 11","Division de 21 par 0"])

-- ================================================================================
-- # Interlude : (non-)commutativité des contextes

-- ### Dans notre dernier exemple, nous avons combiné Writer et Maybe en utilisant
-- ### le type suivant :

-- Writer Log (Maybe Integer)

-- ### L'interprétation de cette combinaison est la suivante :
-- Des calculs optionnels construit dans le cadre d'un log

-- ### On construit des logs de calculs optionnels de la forme :
-- Writer {runWriter = (Just x,[str1, str2, ...])}  -- avec x :: Integer  et str1, str2, ... :: String
-- ### ou
-- Writer {runWriter = (Nothing,[str1, str2, ...])}

-- ### Comparons avec :
-- Maybe (Writer Log Integer)

-- ### Ici les données seraient de la forme :

-- Just (Writer {runWriter = (x, [str1, str2, ...])})
-- ### ou
-- Nothing

-- ### donc il s'agit d'une combinaison probablement moins utile de
-- ### «calculs-loggés» dans un cadre optionnel.

-- ### On retiendra :

-- ## Les combinaisons de contextes ne sont pas (forcément) commutatives

-- ================================================================================
-- # Etape 3 : plongement dans IO

-- ### Sauf cas exceptionnel, en Haskell tout se place toujours «finalement» dans
-- ### le contexte de la monade IO

-- ### Pour notre application fictive, nous considérons les fonctions suivantes :

readInt :: String -> Maybe Integer
readInt str = case (reads str) :: [(Integer,String)] of
                [(k, "")] -> Just k
                _ -> Nothing

-- >>> readInt "42"
-- Just 42


-- >>>readInt "42a.3"
-- Nothing


-- >>> readInt "abc"
-- Nothing


complexOpSafeIO :: IO (Writer Log (Maybe Integer))
complexOpSafeIO = do  -- ici ce `do` est dans IO
  putStr "n = "
  hFlush stdout
  str <- getLine
  return $
    case readInt str of
      Nothing -> return Nothing
      Just n -> do logg $ "Lecture de n=" <> (show n)
                   complexOpSafe n

main1 :: IO ()
main1 = do
  putStrLn "Saisir un entier:"
  Writer (x, log) <- complexOpSafeIO
  putStrLn $ "Resultat = " <> (show x)
  putStrLn $ "Log :"
  mapM (\s -> putStrLn $ "  - " <> s) log
  return ()

-- ### (à tester avec GHCi ou dans main)

-- ================================================================================
-- # Bilan de l'approche «ad hoc»

-- Il n'y a rien a priori «de mal» avec l'approche «ad hoc».
-- Et elle reflète ce que typiquement on ferait en programmation
-- fonctionnelle «de base» (i.e. PAF sans A)

-- Mais notre exemple simpliste ne montre pas qu'en pratique
-- cela peut devenir assez fastidieux de devoir, comme cela,
-- «emboiter» et «déboiter» les contextes de façon purement manuelle.

-- Haskell (et GHC avec ses extensions) nous à habitué à exploiter
-- des concepts d'ordre supérieur pour réduire ce sentiment de
-- code répétitif et «boiler plate» (i.e. PAF avec A)

-- ### Par exemple : récursion explicite ⟶ fold(s) ⟶ Foldable/Traversable ( ⟶ recursion schemes)

-- La question suivante se pose donc :

-- ## Existe-t-il une façon plus générique pour composer
-- ## des contextes ?

-- ### La réponse est un peu «Normande» car on pourrait dire ...

-- ## ... Oui ... mais ...

-- ================================================================================

-- # Composition générique ?

-- De façon assez naturelle, on peut essayer d'écrire
-- un combinateur contextuel générique.

newtype Compose t m a = Compose { getCompose :: t (m a) }
  deriving Show

-- On considère donc :
-- • un type générique `a`,
-- • dans un contexte `m`,
-- • lui-même plongé dans un contexte `t`

-- ## Question : quel est le kind de Compose ?

-- >>> :kind Compose
-- Compose :: (* -> *) -> (* -> *) -> (* -> *)

-- ### Voici un exemple d'une valeur possible avec ce constructeur :

-- >>> Compose $ Writer (Just (42 :: Integer),["Au debut c'est 42"])
-- Compose {getCompose = Writer {runWriter = (Just 42,["Au debut c'est 42"])}}

-- ### Le plus intéressant est le type associé :

-- ...  :: Compose (Writer [[Char])  Maybe   Integer
--                 \______________| |_____| |______|
--                         t           m       a

-- ### Cette composition est-elle préservée par Functor, Applicative et Monad ?
--     ==> réponses (OUI/OUI/NON), cf. TD9


-- ================================================================================
-- # Les Monad Transformers

-- ### Puisque la composition monadique «externe» (avec Compose)
-- ### ne fonctionne pas à 100%, il semble naturel de s'intéresser
-- ### à une approche de combinaison «interne»

-- ## Exemple : le transformer de Maybe

-- Le plus souvent, le constructeur de type `Maybe` se place juste au dessus
-- d'un type «simple», hors-contexte, car on veut juste des «valeurs optionnelles»

-- ### Maybe a

-- En revanche, on souhaite en général ajouter/emboiter des contextes «au dessus»
-- donc on obtient quelque chose de la forme :

-- ## m (Maybe a)
-- où `m` est un contexte monadique quelconque

-- ### C'est exactement le point de départ de la définition du transformer `MaybeT`

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- ## Attention :
-- Pour des raisons pédagogiques, on redéfinit ici `MaybeT` mais
-- la plupart des transformers «de base» sont définis
-- dans la bibliothèque tierce `transformers`.

-- ## Question : quel est le kind de MaybeT ?

-- >>> :kind MaybeT
-- MaybeT :: (* -> *) -> * -> *

-- ### On dit que `MaybeT` est ainsi un «transformateur de contexte»
-- (ou plus simplement un constructeur de contexte)

-- >>> :kind MaybeT []
-- MaybeT [] :: * -> *

-- >>> :kind MaybeT (Writer [String])
-- MaybeT (Writer [String]) :: * -> *

-- ## Attention :
-- ### dans  `MaybeT m a`  le `Maybe` est dans `m` et non l'inverse

-- ================================================================================
-- # MaybeT : contexte fonctoriel

-- ### Notre première étape est de montrer que `MaybeT` préserve les
-- ### contextes fonctoriels

-- >>> :info MaybeT
-- type MaybeT :: (* -> *) -> * -> *
-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap g (MaybeT x) = MaybeT $ fmap (fmap g) x

-- >>> :info MaybeT
-- type MaybeT :: (* -> *) -> * -> *
-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- g :: a -> b
-- x :: m (Maybe a)
-- <<goal>> :: m Maybe b

-- Maybe est un foncteur, donc :
-- fmap/Maybe :: (a -> b) -> (Maybe a -> Maybe b)

-- m  est un foncteur, donc :
-- fmap/m :: (u -> v) -> m u -> m v
-- on aimerait avoir v = Maybe b
-- pour pouvoir utiliser fmap/m, on pose u = Maybe a
-- on aurait alors : fmap/m :: (Maybe a -> Maybe b) -> m Maybe a -> m Maybe b 

-- or,  fmap/Maybe g :: Maybe a -> Maybe b

-- ## Exemples :

safeVal :: Monad m => a -> m (Maybe a)
safeVal = pure . Just

-- >>> runMaybeT $ fmap (*2) (MaybeT (safeVal 42))
-- Just 84

-- >>> runMaybeT $ fmap (*2) (MaybeT (fmap Just [1..4]))
-- [Just 2,Just 4,Just 6,Just 8]

-- ================================================================================
-- # MaybeT : contexte applicatif

-- Pour `Applicative`, sans surprise c'est un peu plus complexe

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  -- pure x = MaybeT $ pure (pure x)
  pure = MaybeT . pure . pure

-- x :: a
-- _goal1 :: m Maybe a

-- pure/Maybe  ::  a -> Maybe a
-- pure/m :: u -> m u
-- si u = Maybe a  alors pure/m :: Maybe a -> m Maybe a

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT g) <*> (MaybeT x) = MaybeT $ (fmap (<*>) g) <*> x 

-- g :: m Maybe (a -> b)
-- x :: m Maybe a
-- <<_goal2>> m Maybe b

-- <*>/Maybe :: Maybe (a -> b) -> (Maybe a -> Maybe b)

-- fmap/m :: (u -> v) -> m u -> m v
-- avec u= Maybe (a -> b) et v=(Maybe a -> Maybe b)
-- donc (fmap/m <*>/Maybe) ::  m Maybe (a -> b) -> m (Maybe a -> Maybe b)

-- <*>/m :: m (x -> y) -> m x -> m y
-- il faudrait que y=Maybe b donc x=Maybe a
-- et donc <*>/m :: m (Maybe a -> Maybe b) -> m Maybe a -> m Maybe b

-- ## Exemples

-- >>> runMaybeT $ (*) <$> (MaybeT (safeVal 13)) <*> (MaybeT (safeVal 3))
-- Just 39

-- >>> runMaybeT $ (*) <$> (MaybeT (fmap Just [1..4])) <*> (MaybeT (fmap Just [2, 10]))
-- [Just 2,Just 10,Just 4,Just 20,Just 6,Just 30,Just 8,Just 40]

-- ================================================================================
-- # MaybeT : contexte monadique

-- La notation do ... rend les choses un peu plus simples
-- dans Monad que dans Applicative :

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  v >>= f =  MaybeT $ do -- contexte m Maybe
    x <- runMaybeT v
    case x of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

-- v : MaybeT m a
-- f : a -> MaybeT m b

-- ================================================================================
-- # Retour sur l'exemple

-- ### On peut redéfinir nos fonctions du début en
-- ### exploitant MaybeT

-- ### par exemple :

-- incrWithLog :: Integer -> Writer Log Integer
-- incrWithLog x = do
--   logg $ "Increment de " <> (show x)
--   return $ x + 1

incrWithLogM :: Integer -> MaybeT (Writer Log) Integer
incrWithLogM x = MaybeT $ do -- contexte (Writer Log Maybe) 
  logg $ "Increment de " <> (show x)
  return $ Just $ x + 1

-- >>> runMaybeT $ incrWithLogM 10
-- Writer {runWriter = (Just 11,["Increment de 10"])}

-- addWithLog :: Integer -> Integer -> Writer Log Integer
-- addWithLog x y = do
--  logg $ "Addition de " <> (show x) <> " et " <> (show y)
--  return $ x + y

addWithLogM :: Integer -> Integer -> MaybeT (Writer Log) Integer
addWithLogM x y = MaybeT $ do  -- contexte (Writer Log Maybe)
  logg $ "Addition de " <> (show x) <> " et " <> (show y)
  return $ Just $ x + y

-- >>> runWriter $ runMaybeT $ addWithLogM 39 3
-- (Just 42,["Addition de 39 et 3"])

-- divWithLogSafe :: Integer -> Integer -> Writer Log (Maybe Integer)
-- divWithLogSafe x y = do
--   logg $ "Division de " <> (show x) <> " par " <> (show y)
--   return $ x `divSafe` y

divWithLogSafeM :: Integer -> Integer -> MaybeT (Writer Log) Integer
divWithLogSafeM x y = MaybeT $ do  -- contexte (Writer Log Maybe)
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `divSafe` y

-- >>> runWriter $ runMaybeT $ divWithLogSafeM 39 3
-- (Just 13,["Division de 39 par 3"])

-- >>> runWriter $ runMaybeT $ divWithLogSafeM 39 0
-- (Nothing,["Division de 39 par 0"])

-- complexOpSafe :: Integer -> Writer Log (Maybe Integer)
-- complexOpSafe n = do
--   x <- incrWithLog n
--   y <- addWithLog n x
--   divWithLogSafe y $ x `div` 20

complexOpSafeM :: Integer -> MaybeT (Writer Log) Integer
complexOpSafeM n = do -- contexte MaybeT (Writer Log)
  x <- incrWithLogM n
  y <- addWithLogM n x
  divWithLogSafeM y $ x `div` 20

-- >>> runWriter $ runMaybeT $ complexOpSafeM 42
-- (Just 42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])

-- >>> runWriter $ runMaybeT $ complexOpSafeM 10
-- (Nothing,["Increment de 10","Addition de 10 et 11","Division de 21 par 0"])

-- ================================================================================
-- # Changement de niveau lift et hoist

-- ### Lorsque l'on empile des transformers, le `do ...` s'applique
-- ### à un niveau de contexte spécifique

-- ### Par exemple :

-- incrWithLogM :: Integer -> MaybeT (Writer Log) Integer
-- incrWithLogM x = MaybeT $ do -- contexte (Writer Log Maybe) 
--   logg $ "Increment de " <> (show x)
--   return $ Just $ x + 1

-- Ici on a choisi de travailler dans le Writer pour pouvoir logger
-- et il nous a fallu réinjecter le type Maybe manuellement
-- (alors que MaybeT est un déjà un contexte Maybe)

-- On aimerait un moyen de travailler un peu plus simplement dans le contexte
-- «racine», et pouvoir «descendre» à la demande ...

-- ### C'est ce que permet la typeclass suivante :

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- ### L'instanciation pour MaybeT est relativement simple :

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift x = MaybeT $ fmap Just x

-- x :: m a
-- <<goal4>> :: m (Maybe a)

-- fmap/m :: (u -> v) -> m u -> m v
-- (fmap/m Just) :: m a -> m Maybe a
-- avec u~a et v~Maybe a

-- ### Une autre opération intéressante est celle qui permet d'injecter
-- ### un `Maybe` «normal» dans le cadre d'un `MaybeT`

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- ### On peut maintenant réécrire nos derniers exemples de façon un peu plus
-- ### simple, en tout cas en prenant `do ...` dès la «racine» :

-- incrWithLogM :: Integer -> MaybeT (Writer Log) Integer
-- incrWithLogM x = MaybeT $ do -- contexte (Writer Log Maybe) 
--   logg $ "Increment de " <> (show x)
--   return $ Just $ x + 1

incrWithLogT :: Integer -> MaybeT (Writer Log) Integer
incrWithLogT x = do -- contexte MaybeT (Writer Log)
  lift $ logg $ "Increment de " <> (show x)
  return $ x + 1

-- >>> runMaybeT $ incrWithLogT 10
-- Writer {runWriter = (Just 11,["Increment de 10"])}

-- addWithLogM :: Integer -> Integer -> MaybeT (Writer Log) Integer
-- addWithLogM x y = MaybeT $ do  -- contexte (Writer Log Maybe)
--  logg $ "Addition de " <> (show x) <> " et " <> (show y)
--  return $ Just $ x + y

addWithLogT :: Integer -> Integer -> MaybeT (Writer Log) Integer
addWithLogT x y = do  -- contexte MaybeT (Writer Log)
  lift $ logg $ "Addition de " <> (show x) <> " et " <> (show y)
  return $ x + y

-- >>> runWriter $ runMaybeT $ addWithLogT 39 3
-- (Just 42,["Addition de 39 et 3"])

divWithLogSafeT :: Integer -> Integer -> MaybeT (Writer Log) Integer
divWithLogSafeT x y = do  -- contexte MaybeT (Writer Log Maybe)
  lift $ logg $ "Division de " <> (show x) <> " par " <> (show y)
  hoistMaybe $ x `divSafe` y

-- >>> runWriter $ runMaybeT $ divWithLogSafeT 39 3
-- (Just 13,["Division de 39 par 3"])

-- >>> runWriter $ runMaybeT $ divWithLogSafeT 39 0
-- (Nothing,["Division de 39 par 0"])

complexOpSafeT :: Integer -> MaybeT (Writer Log) Integer
complexOpSafeT n = do -- contexte MaybeT (Writer Log Maybe)
  x <- incrWithLogT n
  y <- addWithLogT n x
  divWithLogSafeT y $ x `div` 20

-- >>> runWriter $ runMaybeT $ complexOpSafeT 42
-- (Just 42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])

-- >>> runWriter $ runMaybeT $ complexOpSafeT 10
-- (Nothing,["Increment de 10","Addition de 10 et 11","Division de 21 par 0"])

-- ================================================================================
-- # Monad stack et plongement dans IO

-- ### Pour terminer le cours sur les transformers,
-- ### nous allons revenir à l'exemple du début, qui consiste
-- ### en une combinaison à 3 étages :

-- • au 1er étage, on considère des calculs optionnels avec MaybeT

-- • au 2nd étage, on considère du log avec WriterT

-- • au 3ème et dernier étage on considère le plongement dans IO

-- ### Donc la «Monad Stack» qui nous intéresse est de la forme :

-- MaybeT (WriterT Log IO)

-- Ce qui correspond, en gros, à un contexte combiné :

-- IO (Writer Log Maybe)

-- ### Remarque : en pratique, le dernier étage est quasiment toujours
-- ### IO même si parfois on peut souhaiter l'encapsuler (par exemple [IO a])

-- ================================================================================
-- # De Writer à WriterT

-- Nous avons expliqué le passage de Maybe à MaybeT
-- et nous devons faire la même chose pour Writer
-- en partant de la définition de type suivante :

newtype WriterT e m a = WriterT { runWriterT :: m (Writer e a) }

-- ## Exercice : définir la version *transformer* de Writer
-- ### (correction en fin de cours)

-- ### Et on ajoute quelques utilitaires :

hoistWriter :: (Monad m, Monoid e) => Writer e a -> WriterT e m a
hoistWriter = WriterT . pure

tellT :: (Monad m) => [a] -> WriterT [a] m ()
tellT = hoistWriter . tell

loggT :: (Monad m) => a -> WriterT [a] m ()
loggT x = tellT [x]

-- ================================================================================
-- # Monad stack : 1ère et 2ème couches
-- ## MaybeT (Writer T ...)

-- On reprend les définitions depuis le début,
-- sur une base 100% *transformers*.

-- incrWithLogT :: Integer -> MaybeT (Writer Log) Integer
-- incrWithLogT x = do -- contexte MaybeT (Writer Log)
--   lift $ logg $ "Increment de " <> (show x)
--   return $ x + 1

incrWithLogW :: Monad m => Integer -> (WriterT Log m) Integer
incrWithLogW x = do
  loggT $ "Increment de " <> (show x)
  return $ x + 1

-- >>> runWriterT $ incrWithLogW 10
-- Writer {runWriter = (11,["Increment de 10"])}

addWithLogW :: Monad m => Integer -> Integer -> (WriterT Log m) Integer
addWithLogW x y = do
  loggT $ "Addition de " <> (show x) <> " et " <> (show y)
  return $ x + y

-- >>> runWriterT $ addWithLogW 39 3
-- Writer {runWriter = (42,["Addition de 39 et 3"])}

divWithLogSafeMW :: Monad m => Integer -> Integer -> MaybeT (WriterT Log m) Integer
divWithLogSafeMW x y = do
  lift $ loggT $ "Division de " <> (show x) <> " par " <> (show y)
  hoistMaybe $ x `divSafe` y

-- >>> runWriterT $ runMaybeT $ divWithLogSafeMW 39 0
-- Writer {runWriter = (Nothing,["Division de 39 par 0"])}

complexOpSafeMW :: Monad m => Integer -> MaybeT (WriterT Log m) Integer
complexOpSafeMW n = do
  x <- lift $ incrWithLogW n
  y <- lift $ addWithLogW n x
  divWithLogSafeMW y $ x `div` 20

-- >>> runWriterT $ runMaybeT $ complexOpSafeMW 42
-- Writer {runWriter = (Just 42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])}

-- >>> runWriterT $ runMaybeT $ complexOpSafeMW 10
-- Writer {runWriter = (Nothing,["Increment de 10","Addition de 10 et 11","Division de 21 par 0"])}


-- ================================================================================
-- # Monad Stack : Couche IO

-- ### Comme IO est en général le dernier étage des monad stacks,
-- ### on peut plus facilement proposer une aide pour «remonter»
-- ### directement jusque IO depuis n'importe quel contexte de la hiérarchie.
-- ### Pour cela on utilise la typeclass suivante :

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

-- ### Pour la monade IO, c'est très simple puisque l'on est déjà
-- ### «au bon endroit»

instance MonadIO IO where
  liftIO = id

-- ### Pour MaybeT ce n'est pas très compliqué non plus :

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  -- liftIO x = lift (liftIO x)
  liftIO = lift . liftIO
  
-- x :: IO a
-- <<goal>> :: MaybeT m a

-- On a :
-- liftIO/m :: IO a -> m a
-- Donc :
-- (liftIO/m x) :: m (IO a)

-- Or :
-- lift/(MaybeT m) :: m u -> MaybeT m v
-- Donc avec u~(IO a) et v~a :
-- (lift/(MaybeT m) (liftIO x)) :: MaybeT m a

-- ================================================================================  
-- # Monad stack : programme principal

-- ### Rappel:
-- complexOpSafeIO :: IO (Writer Log (Maybe Integer))
-- complexOpSafeIO = do  -- ici ce `do` est dans IO
--   putStr "n = "
--   hFlush stdout
--   str <- getLine
--   return $
--     case readInt str of
--       Nothing -> return Nothing
--       Just n -> do logg $ "Lecture de n=" <> (show n)
--                    complexOpSafe n

complexOpSafeIO2 :: MaybeT (WriterT Log IO) Integer
complexOpSafeIO2 = do
  liftIO $ putStr "n = "
  liftIO $ hFlush stdout
  str <- liftIO getLine
  x <- hoistMaybe $ readInt str
  lift $ loggT $ "Lecture de n=" <> (show x)
  complexOpSafeMW x

-- ### Rappel :
-- main1 :: IO ()
-- main1 = do
--   putStrLn "Saisir un entier:"
--   Writer (x, log) <- complexOpSafeIO
--   putStrLn $ "Resultat = " <> (show x)
--   putStrLn $ "Log :"
--   mapM (\s -> putStrLn $ "  - " <> s) log
--   return ()

main2 :: IO ()
main2 = do
  Writer (x, log) <- runApp complexOpSafeIO2
  putStrLn $ "Resultat = " <> (show x)
  putStrLn $ "Log :"
  mapM (\s -> putStrLn $ "  - " <> s) log
  return ()

-- ### avec :
runApp :: MaybeT (WriterT e m) a -> m (Writer e (Maybe a))
runApp = runWriterT . runMaybeT
  
-- ================================================================================
-- # Conclusion

-- Dans ce cours, nous avons étudié la composition des contextes

-- • soit de façon «ad hoc», et qui correspond à un point de passage nécessaire

-- • soit de façon un peu plus générique en utilisant des transformers MaybeT, WriterT, etc.

-- • et en TD on travaillera sur la composition générique avec Compose
-- qui s'applique à Functor et Applicative mais (malheureusement) pas à Monad

-- ### L'étape suivante serait d'utiliser des bibliothèques tierces pour éviter
-- ### de réinventer la roue ...  En particulier :

-- • la bibliothèque *transformers* qui fournit les définitions de base
-- (MaybeT, WriterT, StateT, ... MonadTrans, MonadIO, etc.)

-- • la bibliothèque *mtl* (monad transformers library)
-- qui introduit le concept de «dépendance fonctionnelle» et qui permet
-- de rendre les transformers beaucoup plus génériques
-- ==> il s'agit d'une sorte de «standard de fait» dans le monde Haskell
--     un peu la porte d'entrée du niveau avancé en Haskell...

-- ### En TME, nous utiliserons juste *transformers* et, comme vous le verrez
-- ### il est plus simple d'utiliser les transformers que de les reconstruire...

-- Il existe également une approche alternative, plus récente, pour la composition
-- contextuelle : les effets algébriques.

-- • cf. la bibliothèque *fused-effects* : <https://github.com/fused-effects/fused-effects>

-- • ou la bibliothèque *polysemy* : <https://github.com/polysemy-research/polysemy>

-- ==> ce sujet reste du domaine de la recherche ...

-- ================================================================================
-- # Annexe - Solution de l'exercice : WriterT

-- ## SPOILER ALERT
-- ## ==> il est beaucoup plus intéressant d'essayer de le faire
-- ##     que de «lire» la solution proposée ci-dessous

-- ### Rappel :

-- newtype WriterT e m a = WriterT { runWriterT :: m (Writer e a) }

-- ### Dans l'ordre il nous faut :

-- ### • instancier Functor

-- ### • instancier Applicative

-- ### • instancier Monad

-- ### • instancier MonadTrans

-- ### • instancier MonadIO

-- ================================================================================
-- # WriterT : solution

instance Functor m => Functor (WriterT e m) where
  fmap :: (a -> b) -> WriterT e m a -> WriterT e m b
  fmap g (WriterT x) = WriterT (fmap (fmap g) x)

instance (Applicative m, Monoid e) => Applicative (WriterT e m) where
  pure :: a -> WriterT e m a
  pure = WriterT . pure . pure

  (<*>) :: WriterT e m (a -> b) -> WriterT e m a -> WriterT e m b
  (WriterT g) <*> (WriterT x) = WriterT $ ((fmap (<*>)) g) <*> x

instance (Monad m, Monoid e) => Monad (WriterT e m) where
  (>>=) :: WriterT e m a -> (a -> WriterT e m b) -> WriterT e m b
  v >>= f =  WriterT $ do
    Writer (x,!y1) <- runWriterT v
    Writer (x',!y2) <- runWriterT (f x)
    return $ Writer (x',y1 <> y2)

-- f :: a -> WriterT e m b
-- Writer (x,y) :: Writer e a
-- donc :
-- x :: a
-- y :: e

-- f x :: WriterT e m b
-- runWriterT (f x) :: Writer e b 
-- x' :: b
-- y2 :: e

instance Monoid e => MonadTrans (WriterT e) where
  lift :: Monad m => m a -> WriterT e m a
  lift = WriterT . (fmap pure)

instance (Monoid e, MonadIO m) => MonadIO (WriterT e m) where
  liftIO :: IO a -> WriterT e m a
  liftIO = lift . liftIO

