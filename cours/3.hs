
module Cours3_Script where

-- # PAF 3 : Modélisation avec les types III
--           (a.k.a. Type-Driven Development)

-- ## - Principes de Modélisation Fonctionnelle
--      (Functional Domain Modelling)

-- ## - Structures discrètes
--      Séquences, Tables associatives et Ensembles

-- ### ------------------------------------------------
-- ### Copyright © 2024, Frédéric Peschanski
-- ###                 (Sorbonne Université/LIP6)
-- ###   (tous droits réservés)

import Data.Maybe (fromJust)

(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True

infixr 1 ==>


-- =======================================================
-- # Première partie :

-- # Principes de Modélisation Fonctionnelle

-- ## Objectifs

-- ### - utiliser le langage pour modéliser
--       (et pas «juste» programmer)

-- ### - en exploitant le langage des types
--       (mais pas seulement : propriétés, fonctions «undefined»)

-- ### - et en suivant quelques principes méthodologiques
--       (et techniques)

-- =======================================================
-- # Bibliographie

--               __...--~~~~~-._   _.-~~~~~--...__
--             //               `V'               \\
--            //                 |                 \\
--           //__...--~~~~~~-._  |  _.-~~~~~~--...__\\
--          //__.....----~~~~._\ | /_.~~~~----.....__\\
--         ====================\\|//====================
--                         dwb `---`

-- ## Grokking Simplicity
-- ### Taming complex software with functional thiking
-- Eric Normand - Manning - 2021

-- ## Domain Modeling made Functional
-- ### Tackle software complexity with Domain-Driven Design (DDD) with F#
-- Scott Wlaschin - PragProg - 2018

-- ## Modeling in Event-B
-- ### System and Software Engineering
-- Jean-Raymond Abrial - Cambridge University Press - 2010

-- =======================================================
-- # ╭────────────┬─────────────────╮
-- # │ Principe 0 │ Séparation      │
-- # │            │ du Modèle       │
-- # ╰────────────┴─────────────────╯

-- [[[(image :type imagemagick :file "3.png" :width 800)]]]

-- =======================================================
-- # ╭────────────┬──────────────────────╮
-- # │ Principe 1 │ Entités              │
-- # │            │ = sommes de produits │
-- # │            │   (et/ou records)    │
-- # ╰────────────┴──────────────────────╯                                                                                      -- =======================================================-- ## On distingue :

-- ## - les entités du domaine
--      Représentation d'un artefact «métier»

-- ## - les données/valeurs «techniques»
--      • code réutilisé
--      • utilisation d'une bibliothèque tierce
--      • etc.

-- =======================================================
-- ## Exemple d'entité : la cuve
-- (encore !?)

data Cuve =
  Cuve Integer Integer
  | CuvePleine Integer
  | CuveVide Integer
  deriving (Show, Eq)

-- ### Remarque : égalité structurelle nécessaire pour pouvoir tester les états

capacite :: Cuve -> Integer
capacite (Cuve _ cap) = cap
capacite (CuvePleine cap) = cap
capacite (CuveVide cap) = cap

quantite :: Cuve -> Integer
quantite (Cuve qty _) = qty
quantite (CuvePleine cap) = cap
quantite (CuveVide cap) = 0

-- =======================================================
-- # ╭────────────┬────────────────────╮
-- # │ Principe 2 │ Etats remarquables │
-- # │            │ = constructeurs    │
-- # ╰────────────┴────────────────────╯

-- ## Le nom de constructeur identifie l'état

-- ## Exemple des cuves :

-- data Cuve =
--   Cuve Integer Integer
--   | CuvePleine Integer
--   | CuveVide Integer
--   deriving (Show)

-- ### Etats remarquables

-- Cuve vide :

-- >>> CuveVide 3
-- CuveVide 3

-- Cuve pleine :

-- >>> CuvePleine 3
-- CuvePleine 3

-- Autre état :

-- >>> Cuve 2 3
-- Cuve 2 3

-- =======================================================
-- # ╭────────────┬───────────────────────╮
-- # │ Principe 3 │ Cohérence             │
-- # │            │ = Invariant (d'état)  │
-- # ╰────────────┴───────────────────────╯

-- ### Invariant d'état = propriété devant être vraie
-- ### dans tous les états possibles de l'entité

-- ### Etat possible = Etat atteignable/observable par l'utilisateur

-- >>> quantite (Cuve 3 3) == 3
-- True

-- >>> quantite (Cuve 0 3) == 0
-- True

-- ## Exemple : invariant des cuves

prop_inv_Cuve :: Cuve -> Bool
prop_inv_Cuve (Cuve qty cap) = 0 <= qty && qty <= cap
prop_inv_Cuve (CuveVide cap) = cap > 0
prop_inv_Cuve (CuvePleine cap) = cap > 0

-- >>> prop_inv_Cuve (Cuve 3 3)
-- True

-- >>> prop_inv_Cuve (CuveVide 3)
-- True

-- =======================================================


-- ## Remarque : les implémenteurs (et testeurs) peuvent
-- ##            «casser» (temporairement) les invariants

-- >>> prop_inv_Cuve $ CuveVide (-3)
-- False

-- >>> prop_inv_Cuve (Cuve 4 3)
-- False

-- ### On parle d'états incohérents transitoires (transient)


-- =======================================================
-- # ╭────────────┬──────────────────────╮
-- # │ Principe 4 │ Etats incohérents    │
-- # │            │ non constructibles   │
-- # │            │ (dans l'API)         │
-- # ╰────────────┴──────────────────────╯

-- ## Vue fournisseur => constructeurs internes

-- >>> Cuve 3 5
-- Cuve 3 5

-- >>> Cuve 5 5
-- Cuve 5 5

-- >>> Cuve 5 3
-- Cuve 5 3

-- >>> prop_inv_Cuve $ Cuve 5 3
-- False

-- ### Optionnellement, gestion d'erreur pour dénicher les bugs éventuels
-- (mais il s'agit plutôt un anti-pattern en général)

mkCuve :: Integer -> Integer -> Cuve -- (ou avec Either et cas d'erreur)
mkCuve vol cap | cap <= 0 = error "cap <= 0"
               | vol < 0 = error "vol < 0"
               | vol > cap = error "vol > cap"
               | vol == 0 = CuveVide cap
               | vol == cap = CuvePleine cap
               | otherwise = Cuve vol cap

-- >>> mkCuve 3 5
-- Cuve 3 5

-- >>> mkCuve 5 5
-- CuvePleine 5

-- >>> mkCuve 5 3
-- *** Exception: vol > cap
-- CallStack (from HasCallStack):
--   error, called at /tmp/danteiaSHyK.hs:227:30 in main:Cours3

-- =======================================================
-- ## Vue client => Utilisation de «smart constructors»

initCuve :: Integer -> Maybe Cuve
initCuve cap | cap <= 0 = Nothing
             | otherwise = Just $ CuveVide cap

-- >>> initCuve 5

-- >>> prop_inv_Cuve $ fromJust $ initCuve 5

-- >>> initCuve (-2)

-- variante "fournisseur"  (exemple)
mkCuve' :: Integer -> Integer -> Either String Cuve
mkCuve' qty cap | cap <= 0 = Left "La capacite doit etre strictement positive"
                | qty == 0 = Right $ CuveVide cap
                | qty == cap = Right $ CuvePleine cap
                | qty > cap = Left "La quantite dépasse la capacite"
                | otherwise = Right $ Cuve qty cap

-- >>> mkCuve' 3 5
-- Right (Cuve 3 5)

-- >>> mkCuve' 0 5
-- Right (CuveVide 5)

-- >>> mkCuve' 5 5
-- Right (CuvePleine 5)

-- >>> mkCuve' 5 0
-- Left "La capacite doit etre strictement positive"

-- >>> mkCuve' 5 3
-- Left "La quantite d\233passe la capacite"

-- ## Différences
-- ### - data-constructor: «libre» (à éviter dans l'API)
-- ### - smart constructor interne (fournisseur): état incohérent => bug ou état transitoire
-- ### - smart constructor externe (client, API): état incohérent non-constructible

-- =======================================================
-- # ╭────────────┬──────────────────────────────────╮
-- # │ Principe 5 │ Opérations (changements d'états) │
-- # │            │ = préconditions                  │
-- # │            │   + fonctions                    │
-- # │            │   + postconditions               │
-- # ╰────────────┴──────────────────────────────────╯

-- ## Exemple : remplissage d'une cuve

-- ## Précondition : condition d'application cohérente d'une application

prop_pre_fillCuve :: Cuve -> Integer -> Bool
prop_pre_fillCuve cuve vol = (vol > 0) && ((quantite cuve) + vol) <= (capacite cuve)   

-- ## Opération (changement d'état, transition)

-- ### -- version implémenteur (interne)
fillCuveImpl :: Cuve -> Integer -> Cuve
fillCuveImpl (CuveVide cap) vol = mkCuve vol cap
fillCuveImpl (CuvePleine cap) vol = error "Cuve pleine"
fillCuveImpl (Cuve v1 cap) v2 = mkCuve (v1 + v2) cap

-- >>> fillCuveImpl (mkCuve 3 5) 1
-- Cuve 4 5

-- >>> fillCuveImpl (mkCuve 4 5) 1
-- CuvePleine 5

-- >>> fillCuveImpl (mkCuve 5 5) 1
-- *** Exception: Cuve pleine
-- CallStack (from HasCallStack):
--   error, called at /tmp/danteiaSHyK.hs:306:37 in main:Cours3

-- >>> fillCuveImpl (Cuve 3 4) 2
-- Cuve 5 4

-- >>> prop_inv_Cuve (Cuve 5 4)
-- False

-- -- Du côté fournisseur, on peut bien sûr construire des états incohérents (si nécessaire)

-- ### version utilisateur (API externe)
fillCuve :: Cuve -> Integer -> Maybe Cuve
fillCuve cuve vol
  | prop_pre_fillCuve cuve vol == True = Just $ fillCuveImpl cuve vol
  | otherwise = Nothing

-- >>> fillCuve (mkCuve 3 5) 1
-- Just (Cuve 4 5)

-- >>> fillCuve (mkCuve 3 5) 2
-- Just (Cuve 5 5)

-- >>> fillCuve (mkCuve 5 5) 1
-- Nothing

-- ## Postcondition : observations cohérentes de l'opération
-- ==> utilisation pour le test fonctionnel

-- IMPORTANT : on fait l'hypothèse des invariants d'états et des préconditions
-- d'opération dans les postconditions

prop_post_fillCuve :: Cuve -> Cuve -> Integer -> Bool
prop_post_fillCuve cuve cuve' vol =
  quantite cuve' == (quantite cuve) + vol

-- -- ici,  cuve  correspond au pré-état  et cuve' correspond au post-état

-- >>> prop_post_fillCuve (mkCuve 3 5) (fromJust $ fillCuve (mkCuve 3 5) 1) 1
-- True

-- >>> prop_post_fillCuve (mkCuve 3 5) (fillCuveImpl (mkCuve 3 5) 1) 1
-- True

-- ### ==> Important : un test de postcondition faux signale un bug d'implémentation

-- Bien sûr, on test en général des propriétés moins précises

prop_post_fillCuve2 :: Cuve -> Cuve -> Integer -> Bool
prop_post_fillCuve2 cuve cuve' vol =
  quantite cuve' > (quantite cuve)

-- >>> prop_post_fillCuve2 (mkCuve 3 5) (fromJust $ fillCuve (mkCuve 3 5) 1) 1
-- True

-- >>> prop_post_fillCuve2 (mkCuve 3 5) (fillCuveImpl (mkCuve 3 5) 1) 1
-- True

-- ## postcondition de «cohérence» : préservation des invariants
-- (en supposant les invariants et préconditions vérifiés sur le pré-état)

-- >>> prop_inv_Cuve $ fromJust $ fillCuve (mkCuve 3 5) 1

-- >>> prop_inv_Cuve $ fillCuveImpl (mkCuve 3 5) 1


-- =======================================================
-- # ╭────────────┬────────────────────────────╮
-- # │ Principe 6 │ Sémantique                 │
-- # │            │ = Propriétés algébriques   │
-- # │            │   + Property-based testing │
-- # ╰────────────┴────────────────────────────╯

-- ## Propriétés algébriques des opérations

-- ### Idée de base : réfléchir aux propriétés des compositions (possibles) d'opérations 

-- ### Exemple 1 : additivité du remplissage (propriété interne)

prop_fillCuve_additive :: Cuve -> Integer -> Integer -> Bool
prop_fillCuve_additive cuve vol1 vol2 =
  prop_pre_fillCuve cuve (vol1 + vol2)
  ==> (fillCuveImpl (fillCuveImpl cuve vol1) vol2 == fillCuveImpl cuve (vol1 + vol2))

-- >>> prop_fillCuve_additive (mkCuve 2 8) 2 3
-- True

-- >>> prop_fillCuve_additive (mkCuve 2 8) 2 4
-- True

-- >>> prop_fillCuve_additive (mkCuve 2 8) 2 9
-- True

-- ### Exemple 2 : remplissage complet de la cuve

-- Vérification d'un remplissage complet

prop_fillCuve_full :: Cuve -> Cuve -> Integer -> Bool
prop_fillCuve_full cuve cuve' vol
    | vol == capacite cuve - quantite cuve = estPleine cuve'
    | otherwise = False
    where estPleine :: Cuve -> Bool
          estPleine (CuvePleine _) = True
          estPleine _ = False

-- >>> prop_fillCuve_full (CuveVide 5) (fillCuveImpl (CuveVide 5) 5) 5
-- True



-- =======================================================
-- # Exercice (1) : opération de déversement

pumpCuveImpl :: Cuve -> Integer -> Cuve
pumpCuveImpl cuve vol = undefined

pumpCuve :: Cuve -> Integer -> Maybe Cuve 
pumpCuve cuve vol = undefined

-- ## Préconditions et postconditions ?

-- ## Propriétés algébriques ?

-- ## Exemple : idempotence remplissage/pompage

prop_fillpump_idem :: Cuve -> Integer -> Bool
prop_fillpump_idem cuve vol =
    prop_pre_fillCuve cuve vol
    ==> pumpCuveImpl (fillCuveImpl cuve vol) vol == cuve

-- =======================================================

-- # Exercice (2) : volume ≠ entier

-- On a utilisé le type Integer pour représenter les volumes
-- et on a toujours dû vérifier que l'entier était bien positif

-- ### Ne peut-on pas rendre le programme intrinsèquement plus sûr ?
-- ==> Principe 4 : états incohérents non-constructibles

-- Une pratique courante est d'introduire un "wrapper" pour le type concerné

newtype Volume = Volume Integer
  deriving (Show, Eq, Ord)

-- Smart constructor
vol :: Integer -> Volume
vol n | n < 0 = error "Volume négatif"
      | otherwise = Volume n

-- => Mettre à jour les cuves avec ce nouveau type

-- ## Remarque :  newtype introduit un type "zero-cost abstraction"
-- (contrairment à Data)


