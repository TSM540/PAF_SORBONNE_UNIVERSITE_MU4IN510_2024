
{-# LANGUAGE BangPatterns #-}

module Cours4_Script where

-- # PAF 4 : Evaluation non-stricte

-- ## - Stratégies d'évaluation non-strictes

-- ## - contrôle de la stratégie
--      (rudiments)

-- ## - Applications pratiques

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                  (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

-- -- on aura besoin de la version stricte du fold left ...
import Data.Foldable (foldl')
import Control.Concurrent (yield)

-- ===========================================================================
-- # Stratégies d'évaluation non-strictes

-- ## Rappel : Haskell est un langage à expression

-- ## Schéma d'évaluation :

--                                ┌──────────────────────┐
-- Expression :: Type  ──────────►│                      ├───────► Valeur (:: Type)
--                                │     Evaluation       │
-- Exemple:                       │                      │
--   2 + 3 :: Integer  ──────────►│                      ├───────►  5 (:: Integer)
--                                └──────────────────────┘

-- ## Il y a plusieurs stratégies d'évaluation possibles :

-- ### - stratégie stricte avec appels par valeur
--   (Ocaml, Scheme, Scala par défaut, etc.)

-- ### - stratégie non-stricte avec appels par nom
--    (Scala, en option)

-- ### - stratégie non-stricte paresseuse (appels par nécessité)
--      (Haskell, assistants de preuve ...)

-- - (etc.)

-- ===========================================================================
-- # Strict vs. non-strict : les bases

-- On considère les fonctions suivantes :

square :: Integer -> Integer
square z = z * z

dist :: Integer -> Integer -> Integer
dist x y = square x + square y

-- ### Regardons l'évaluation de :

-- >>> dist (2 * 2) (1 + 1)
-- 20

-- ## Evaluation stricte «par valeur»
--    (call-by-value)

-- dist (2 * 2) (1 + 1)
-- ### {évaluation des arguments}
-- ==> dist 4 2             
-- ### {appel de la fonction}
-- ==> square 4 + square 2
-- ==> 4 * 4 + 2 * 2
-- ==> 20

-- ## Evaluation non-stricte «par nom»
--    (call-by-name)

-- dist (2 * 2) (1 + 1)
-- {appel de la fonction}
-- ==> square (2 * 2) + square (1 + 1)
-- {appel de la fonction}
-- ==> (2 * 2) * (2 * 2) + (1 + 1) * (1 + 1)
-- ==> 20

-- ## Remarques
-- - le résultat obtenu est le même
-- - l'appel par valeur est plus efficace ici

-- ===========================================================================
-- # Stratégie non-stricte paresseuse

-- ## Principes de l'évaluation paresseuse
-- - ne jamais évaluer une expression si on n'a pas
--   besoin de la valeur
--   (==> call-by-need)
-- - ne pas évaluer plus d'une fois la même expression

-- ### Exemple :

-- dist (2 * 2) (1 + 1)
-- ### {thunking}
-- ≡ dist <X> <Y>                 -- {<X>⇝(2*2), <Y>⇝(1+1)}
-- ### {appel de la fonction}
-- ==> square <X> + square <Y>    -- {<X>⇝(2*2), <Y>⇝(1+1)}
-- ### {appel de la fonction}
-- ==> <X> * <X> + <Y> * <Y>      -- {<X>⇝(2*2), <Y>⇝(1+1)}
-- ### {évaluation des besoins}
-- ==> <X> * <X> + <Y> * <Y>      -- {<X>⇝(2*2), <Y>⇝(1+1){2}}
-- ==> 4 * <X> + <Y> * <Y>        -- {<X>⇝(2*2){4}, <Y>⇝(1+1)}
-- ==> 4 * 4 + <Y> * <Y>          -- {<X>⇝(2*2){4}, <Y>⇝(1+1)}
-- ==> 4 * 4 + 2 * <Y>            -- {<X>⇝(2*2){4}, <Y>⇝(1+1){2}}
-- ==> 4 * 4 + 2 * 2              -- {<X>⇝(2*2){4}, <Y>⇝(1+1)}
-- ==> 20

-- ## Important
-- - le thunking consiste à nommer/partager une expression non-évaluée
-- - la valeur finale est (encore une fois) la même
-- - Haskell utilise une stratégie mixte
--   (lazy par défaut + annotation de «strictness»)

-- ===========================================================================
-- # Contrôle de la stratégie d'évaluation

-- En première approche, il est conseillé de «laisser le compilateur faire son travail»
-- ... mais il est parfois utilse de pouvoir influencer la stratégie d'évaluation.
-- (==> dans ce cours : quelques notions de base)

-- # 1) Arguments non-stricts vs. stricts

-- Lorsque l'on implémente une fonction, les arguments sont considérés comme non-stricts par
-- défaut.  Mais on peut «forcer la strictness»

data PNat = Z | S PNat
  deriving (Show, Eq)

one, two, three :: PNat
one = S Z
two = S one
three = S two

-- ## Version paresseuse (par défaut)

lazyPlus :: PNat -> PNat -> PNat
lazyPlus Z n = n
lazyPlus (S m) n = S $ lazyPlus m n


-- lazyPlus three two
-- ==> S (lazyPlus two two)
-- ==> S (S (lazyPlus one two))
-- ==> S (S (S (lazyPlus Z two)))
-- ==> S (S (S two))
-- ==> S (S (S (S (S Z))))

-- ===========================================================================
-- # Indications de "strictness" (avec seq)

-- ## Opérateur d'évaluation stricte:

-- >>> :t seq
-- seq :: a -> b -> b

-- ### seq e1 e2  (ou  e1 `seq` e2)

-- signifie:

-- ### l'expression e1 doit être évaluée e1 pour obtenir le résultat de e2
-- ### (et l'évaluation de e1 est "forcée")

-- un peu plus précisément:

-- ### la valeur de e1 est demandée pour évaluer e2

-- Exemple : deuxième argument strict
seqPlus :: PNat -> PNat -> PNat
seqPlus Z n = n
seqPlus (S m) n = n `seq` S (seqPlus m n)
-- la valeur de n est demandée pour évaluer `S (seqPlus m n)`

-- seqPlus three two
-- ==> S (seqPlus two (S (S Z)))
-- ==> S (S (seqPlus one (S (S Z))))
-- ==> S (S (S (seqPlus Z (S (S Z)))))
-- ==> S (S (S (S (S Z))))

-- Attention : le `seq` peut être contre-intuitif, notamment
-- dans l'exemple suivant :

add :: Int -> Int -> Int
add x y = x `seq` x + y
-- ici on indique: x est demandée pour calculer x + y
-- mais x + y demande déjà x donc c'est redondant,

-- Le programme suivant est donc identique :
add' :: Int -> Int -> Int
add' x y = x + y

-- ===========================================================================
-- # Variante : le bang pattern (!)

-- L'extension de language {-# BangPatterns #-}
-- permet d'indiquer explicitement dans le pattern (lhs) d'une
-- équation que la valeur d'une variable (ou expression à variables)
-- est demandée par la rhs (partie droite).

-- Dans un pattern, pour un argument x on utilise l'écriture !x

-- exemple : deuxième argument strict
bangPlus :: PNat -> PNat -> PNat
bangPlus Z n = n
bangPlus (S m) !n = S $ bangPlus m n

-- ici la valeur de n est demandée pour calculer la partie droite
-- on peut reproduire la même dépendance avec un let

bangPlus' :: PNat -> PNat -> PNat
bangPlus' Z n = n
bangPlus' (S m) n = let !k = n
                    in S $ bangPlus m k   -- Q : que se passe-t-il si on laisse n plutôt que k ?

-- ===========================================================================
-- # 2) Evaluation dirigée par le pattern-matching

-- ### Question : comment savoir si on a «besoin» d'évaluer une expression ?

-- Les *patterns* expliquent les morceaux qui doivent être évalués lors
-- des appels de fonction,
-- chaque pattern étant testé l'un après l'autre en cas d'échec du précédent.

-- ### Considérons l'exemple suivant :

sumFirst :: [Integer] -> [Integer] -> Integer
sumFirst [] ys = 0
sumFirst (x:xs) [] = 0
sumFirst (x:xs) (y:ys) = x + y

-- Par exemple :

-- sumFirst [] [1+1, 2+2, 3+3]
-- ### {pattern sumFirst.1 : succès} :  
-- ==> sumFirst [] <YS>   -- <YS>⇝[1+1, 2+2, 3+3]
-- ==> 0

-- Ici on n'a pas évalué le thunk <YS>

-- De même :

-- sumFirst [1+1, 2+2, 3+3] []
-- ### {pattern sumFirst.1 : <échec>}
-- ### {pattern sumFirst.2 : <succès>}
-- ==> sumFirst (<X>:<XS>) []  -- <X>⇝1+1, <XS>⇝ [2+2, 3+3] 
-- ==> 0

-- Et finalement :

-- sumFirst [1+1, 2+2, 3+3] [1*1, 2*2, 3*3]
-- ### {pattern sumFirst.1 : <échec>}
-- ### {pattern sumFirst.2 : <échec>}
-- ### {pattern sumFirst.3 : <succès>}
-- ==> sumFirst (<X>:<XS>) (<Y>:<YS>)   -- <X>⇝1+1, <XS>⇝ [2+2, 3+3] 
--                                      -- <Y>⇝1*1, <YS>⇝ [2*2, 3*3]
-- ==> {opération + strict}
-- ==> <X> + <Y>    -- <X>⇝1+1{2}, <XS>⇝ [2+2, 3+3]
--                  -- <Y>⇝1*1{1}, <YS>⇝ [2*2, 3*3]
-- ==> 2 + 1
-- ==> 3

-- ===========================================================================
-- # Cas d'étude : foldr vs. foldl vs. foldl'

-- Pour bien comprendre cette partie du cours, il faut bien faire
-- la distinction entre deux usages communs des fold:

-- ## 1. les constructions inductives
-- ### exemples : construction d'une liste, d'un arbre, etc.
-- (spoiler => foldr)

-- ## 2. les réductions
-- ### exemples : calcul d'une somme, d'un produit, etc.
-- (spoiler => foldl')
-- ===========================================================================
-- # Le fold right

-- Rappel :

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- Voici une définition spécifique aux listes:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ final [] = final
myFoldr f final (x:xs) = f x (myFoldr f final xs)

-- Dans un langage strict, cette définition est jugée
-- peu efficace car la récursion est non-terminale

-- En fait, cette définition
-- est au contraire utile si l'appel correspond à une
-- construction de structure, en particulier si les
-- constructeurs sont paresseux (ce qui est le cas
-- par défaut en Haskell.

-- Par exemple, la version suivante du map est efficace
-- (pour les constructions)

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x xs -> f x : xs) []

-- >>> myMap (*2) [1, 2, 3, 4, 5]
-- [2,4,6,8,10]
-- (0.01 secs, 424,808 bytes)

-- autre exemple «paresseux»

bigList :: [Integer]
bigList = [1..10000000]

-- >>> let l = myMap (*2) bigList in take 10 l
-- [2,4,6,8,10,12,14,16,18,20]
-- (0.00 secs, 438,296 bytes)

-- En revanche, les réductions sont problématiques :

-- >>> myFoldr (+) 0 bigList
-- *** Exception: stack overflow

-- remarque :  [1..1000000] fonctionne !

-- ===========================================================================
-- # Le fold left

-- Rappel:

-- >>> :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- Et la version spécialisée sur le listes :

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ ini [] = ini
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Haskell élimine la plupart des appels terminaux
-- donc on pourrait penser cette version efficace

-- Et en fait non !

-- >>> myFoldl (+) 0 bigList
-- 50000005000000
-- (5.60 secs, 2,416,820,616 bytes)

-- La raison ?  Le thunking ...

-- ===========================================================================
-- # Le fold left  strict pour les réductions

-- Le fold left existe en version stricte, et c'est le fold
-- privilégié pour effectuer des réductions

-- La bibliothèque base fournit une version stricte du fold left :

-- >>> :t foldl'
-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- >>> foldl' (+) 0 bigList
-- 50000005000000

-- On peut tenter une version spécialisée pour les listes :

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' _ ini [] = ini
myFoldl' f acc (x:xs) = let acc' = f acc x
                        in seq acc' $ myFoldl' f acc' xs
-- >>> myFoldl' (+) 0 bigList
-- 50000005000000
-- (3.69 secs, 2,560,423,776 bytes)

-- Remarque : on voit mieux la différence en compilation native

-- La version de base est tout à fait contre-intuitive !

baseFoldl' :: (b -> a -> b) -> b -> [a] -> b
baseFoldl' f z0 xs = foldr f' id xs z0
  where f' x k z = k $! f z x

-- Remarque :  $!  est une "application stricte" dérivée du bang pattern

-- >>> baseFoldl' (+) 0 bigList
-- 50000005000000
-- (3.27 secs, 2,240,423,896 bytes)

-- ===========================================================================
-- # Les folds : à retenir

-- ## pour les constructions inductives

-- Utilisation de foldr  (du prélude)

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- ## pour les réductions

-- Utilisation de foldl'  (dans Data.Foldable de base)

-- >>> :t foldl'
-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- (et pas uniquement pour les listes, pour
--  les «traversables» en général)

-- ===========================================================================
-- # Applications pratiques de la stratégie non-stricte

-- ## 1) évaluation optionnelle

-- ## 2) Composition de fonctions et style «point-free»

-- ## 3) Structures co-inductives et co-récursion
--       Exemple des listes

-- ===========================================================================
-- # 1) Evaluation optionnelle

-- ### La stratégie non-stricte de Haskell évalue:
-- - au plus une fois chaque expression
-- - de plus, une expression non-utilisée n'est pas évaluée

-- Ceci permet de créer, sous forme de fonction, certaines
-- structures de contrôles :

fif :: Bool -> a -> a -> a
fif True ethen _ = ethen
fif False _ eelse = eelse

-- >>> fif True (2+2) (error "boum")
-- 4

-- ## exemples :
-- - and et or paresseux sont des fonctions en Haskell
-- - idem pour l'implication ==> utile pour les tests
-- - opérateurs de contrôle pour le backtrack implicite (ex. parsing)
-- - etc.

-- ===========================================================================
-- # 2) Composition de fonctions

-- ## Rappel : opérateur de composition

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- En évaluation stricte, la composition de fonction n'est
-- pas dissociable de la composition séquentielle des calculs.

-- Exemple de composition :

exCompo :: [Integer] -> Integer
exCompo =  foldl' (+) 0 . fmap  (\x -> x * x) . filter odd

-- >>> exCompo [1..20]
-- 1330

-- En stratégie stricte, à partir d'une liste:
-- 1) filtre les éléments impairs
-- 2) *puis* applique la fonction carré
-- 3) *puis* réduit la somme des éléments

-- ==> il y a donc 3 parcours complets de liste

-- Donc en strict la fonction suivante est équivalente :

exCompo':: [Integer] -> Integer
exCompo' xs =  foldl' (+) 0 (fmap  (\x -> x * x) (filter odd xs))

-- >>> exCompo' [1..20]
-- 1330

-- Mais en stratégie non-stricte:
-- pour chaque élément de la liste :
--  1) si l'élément est impair il est fourni à la fonction "suivante"
--  2) cet élément est élevé au carré
--  3) la somme est accumulée de façon paresseuse

-- ==> il n'y a qu'un seul parcours de liste

-- ===========================================================================
-- # Autre exemple: composition de maps

-- fmap (f . g)

-- ... n'est pas plus rapide que

-- fmap f . fmap g

-- >>> :set +s

compo1 :: [Integer] -> Integer
compo1 = (foldl' (+) 0) . (fmap ((+1) . (*2)))

-- >>> compo1 bigList
-- 100000020000000
-- (3.39 secs, 2,400,425,048 bytes)

compo2 :: [Integer] -> Integer
compo2 = (foldl' (+) 0) . fmap (+1) . fmap (*2)

-- >>> compo2 bigList
-- 100000020000000
-- (3.48 secs, 2,960,425,144 bytes)

-- >>> :unset +s

-- ===========================================================================
-- # 3) Les liste paresseuses

-- De façon implicite, les listes de Haskell sont paresseuses,
-- donc construites «à la demande».
-- ==> cela correspond à la notion de stream dans les langages stricts

-- Exemple : une liste «infinie» de 1

ones :: [Integer]
ones = 1 : ones

-- ## Remarque : absence de cas de base
-- ==> on parle de fonction co-récursive

-- ## Important : il ne faut faut jamais réduire/consommer une list «infinie»

-- Par contre on peut consommer/réduire un préfixe fini

-- >>> head ones
-- 1

-- >>> :t take
-- take :: Int -> [a] -> [a]

-- >>> take 10 ones
-- [1,1,1,1,1,1,1,1,1,1]

-- ===========================================================================
-- # Cas d'étude : les suites d'entiers naturels

genNats :: Integer -> [Integer]
genNats n = n : genNats (n + 1)

nats :: [Integer]
nats = genNats 1

-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

-- ### Remarque : définition par compréhension

nnats :: [Integer]
nnats = [1..]

-- >>> take 10 nnats
-- [1,2,3,4,5,6,7,8,9,10]

-- ### Exemple : (re)définition de `take`

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 1 (x : _) = [x]
myTake n (x : xs) = x : myTake (n - 1) xs

-- >>> myTake 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

-- ===========================================================================
-- # Exercice : iterate

-- La fonction d'itération `iterate` du prélude à la signature suivante :

-- >>> :t iterate

-- iterate f x
-- retourne la liste infinie : `[x, f x, f f x, f f f x, ...]`

-- ### Exercice : les entiers naturels avec iterate

nnnnats :: [Integer]
nnnnats = iterate (+1) 1

-- ### Exercice : (re)définition de iterate

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- >>> take 10 $ myIterate (+1) 1

-- ### Exercice : (re)définition de filter

-- >>> take 10 $ filter odd nats

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
  = let rst = myFilter pred xs
    in if pred x then x : rst
       else rst

-- >>> take 10 $ myFilter odd nats
