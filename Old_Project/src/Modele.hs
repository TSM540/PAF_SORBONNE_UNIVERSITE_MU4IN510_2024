module Modele where

import qualified Data.Map.Strict as M

import qualified System.Random as R

import Data.Random as DR
import Data.Random.Shuffle.Weighted as RSW
import Control.Monad.State.Lazy as MSL

import Carte
import Environnement
import Keyboard

data Modele = Cont {carte :: Carte, -- carte actuelle
                    envi :: Envi, -- environnement actuel
                    gene :: R.StdGen, -- generateur aleatoire
                    log :: String, -- journal
                    keyboard :: Keyboard -- l'etat du clavier
                    } deriving (Show)

data Ordre = N | S | E | O | U | R deriving (Eq, Show)
-- resp. Nord, Sud, Est, Ouest, Utiliser, Rien faire

-- deplace une entite
bouge :: Modele -> Entite -> Coord -> Modele
bouge (Cont carte env seed log keyboard) e coord = 
  (Cont carte (bouge_id (entite_get_id e) coord env) seed (log ++ "entite deplacee") keyboard)


-- decide de l'action a faire suivant une liste ponderee
decide :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decide listeOrdre modele@(Cont c env seed log keyboard) entite = 
  appliquerOrdre ((MSL.evalState (DR.runRVar (RSW.weightedSample 1 listeOrdre) DR.StdRandom) seed) !! 0) entite modele


-- applique un ordre a une entite
appliquerOrdre :: Ordre -> Entite -> Modele -> Modele
appliquerOrdre ordre entite modele@(Cont carte env seed log keyboard) =
  case trouve_id (entite_get_id entite) env of
    Just ((C x y), e) | (ordre == N) -> bouge modele entite (C x (y-1))
                      | (ordre == S) -> bouge modele entite (C x (y+1))
                      | (ordre == E) -> bouge modele entite (C (x+1) y)
                      | (ordre == O) -> bouge modele entite (C (x-1) y)
                      | (ordre == U) -> modele -- a definir
                      | (ordre == R) -> modele
    Nothing -> error "entite non existante"


-- donne la liste d'ordres ponderes pour une vache
prevoit_vache :: Modele -> Entite -> [(Int, Ordre)]
prevoit_vache modele@(Cont carte env seed log keyboard) entite = case trouve_id (entite_get_id entite) env of
  Just (coord, e) -> prevoit_vache_aux (ordresDirectionPossibles modele coord [])
  Nothing -> error "entite non existante"
  where
    prevoit_vache_aux :: [Ordre] -> [(Int, Ordre)]
    prevoit_vache_aux [] = [(2,R)]
    prevoit_vache_aux (o:os) = (prevoit_vache_aux os) ++ [(1,o)]
                                                   
  
-- donne toutes les directions possibles pour une entite qui ne peut pas ouvrir les portes
ordresDirectionPossibles :: Modele -> Coord -> [Ordre] -> [Ordre]
ordresDirectionPossibles (Cont carte env seed log keyboard) (C x y) ordres = do
  let res = if (isPorteOuMur carte (C x (y-1))) == False && franchissable_env (C x (y-1)) env then ordres ++ [N] else ordres
  let res' = if (isPorteOuMur carte (C x (y+1))) == False && franchissable_env (C x (y+1)) env then res ++ [S] else res
  let res'' = if (isPorteOuMur carte (C (x+1) y)) == False && franchissable_env (C (x+1) y) env then res' ++ [E] else res'
  let finalRes = if (isPorteOuMur carte (C (x-1) y)) == False && franchissable_env (C (x-1) y) env then res'' ++ [O] else res''
  finalRes


