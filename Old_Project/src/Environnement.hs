module Environnement where

import qualified Data.Map.Strict as M

import Carte

-- environnement : position des entites
data Envi = Envi {contenu_envi :: M.Map Coord [Entite]}

-- a completer avec d'autres entites pour rajouter du contenu
data Entite = Vache {idEn :: Int, pv :: Int}
              | Joueur {idEn :: Int, pv :: Int}
              deriving (Show)

instance Eq Entite where
  e1 == e2 = (entite_get_id e1) == (entite_get_id e2)


-- invariant de l'environnement
env_inv :: Envi -> Bool
env_inv = undefined -- verifier que les cases existent dans la carte ?


-- precondition pour prop_franchissable
-- on verifie si la case existe
prop_franchissable_pre :: Coord -> Envi -> Bool
prop_franchissable_pre coord env = caseExiste coord env


-- Determine si une case est franchissable
-- True si la case en franchissable, False sinon
franchissable_env :: Coord -> Envi -> Bool
franchissable_env coord (Envi contenu_envi) = case M.lookup coord contenu_envi of
  Just listeEntite -> isListeFranchissable listeEntite
  Nothing -> True
  where
    isListeFranchissable :: [Entite] -> Bool
    isListeFranchissable [] = True
    isListeFranchissable ((Joueur idEn pv):es) = False
    isListeFranchissable ((Vache idEn pv):es) = False
    -- si on ajoute des entite traversables (objets), c'est de la forme
    -- isListeFranchissable ((nomEntite attributs):es) = isListeFranchissable es


-- on verifie si l'id existe au prealable
-- Attention : on exploite le fait que la fonction peut retourner Nothing ailleurs
-- probablement pas utilise
prop_trouve_id_pre :: Int -> Envi -> Bool
prop_trouve_id_pre idEn env = idExiste idEn env


-- trouve une entite et ses coordonnes a partir de son identifiant
trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id idEn (Envi contenu_envi) = M.foldlWithKey' trouve_id_aux Nothing contenu_envi where
  trouve_id_aux :: Maybe (Coord, Entite) -> Coord -> [Entite] -> Maybe (Coord, Entite)
  trouve_id_aux (Just (c, e)) _ _ = Just (c, e)
  trouve_id_aux _ _ [] = Nothing
  trouve_id_aux Nothing coord (e:es) = if idEn == (entite_get_id e) then Just (coord, e)
                                          else trouve_id_aux Nothing coord es


-- precondition, on verifie si l'id existe
prop_rm_env_id_pre :: Int -> Envi -> Bool
prop_rm_env_id_pre idEn env = idExiste idEn env


-- enleve une entite de l'environnement a partir de son identifiant
rm_env_id :: Int -> Envi -> Envi
rm_env_id idEn env@(Envi contenu_envi) = case trouve_id idEn env of 
  Just (coord, entite) -> case M.lookup coord contenu_envi of
    Just listeEntite -> (Envi (M.insert coord (removeElement entite listeEntite) contenu_envi))
    Nothing -> error "coordonnee non existante (???)"
  Nothing -> error "entite non existante"


-- precondition, on verifie que l'id existe
prop_bouge_id_pre :: Int -> Envi -> Bool
prop_bouge_id_pre idEn env = idExiste idEn env


-- deplace une entite a partir de son identifiant
bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id idEn newCoord env@(Envi contenu_envi) = case trouve_id idEn env of 
  Just (coord, entite) -> do
    let newEnv@(Envi newContenu_envi) = rm_env_id idEn env
    ajout_entite entite newCoord newEnv
  Nothing -> error "entite non existante"


-- precondition
-- si la case existe, on verifie qu'elle est franchissable (on peut rajouter qqch dessus)
-- on verifie egalement que l'identifiant n'existe pas deja
prop_ajout_entite_pre :: Entite -> Coord -> Envi -> Bool
prop_ajout_entite_pre e coord env = if caseExiste coord env 
                                    then franchissable_env coord env && (trouve_id (entite_get_id e) env) == Nothing
                                    else (trouve_id (entite_get_id e) env) == Nothing


-- ajoute une entite a l'environnement
ajout_entite :: Entite -> Coord -> Envi -> Envi
ajout_entite e coord (Envi contenu_envi) = case M.lookup coord contenu_envi of
  Just listeEntite -> (Envi (M.insert coord (listeEntite ++ [e]) contenu_envi))
  Nothing -> (Envi (M.insert coord  [e] contenu_envi))


-- recupere l'id d'une entite
entite_get_id :: Entite -> Int
entite_get_id (Joueur idEn _) = idEn
entite_get_id (Vache idEn _) = idEn


-- retire un element d'une liste
removeElement :: (Eq a) => a -> [a] -> [a]
removeElement _ [] = []
removeElement e (x:xs)
  | e == x = removeElement e xs
  | otherwise = x : removeElement e xs


-- verifie si une case existe dans l'environnement
caseExiste :: Coord -> Envi -> Bool
caseExiste coord (Envi contenu_envi) = case M.lookup coord contenu_envi of
  Just c -> True
  Nothing -> False


-- verifie si un identifiant d'entite existe ou non
-- semblable a trouve_id 
idExiste :: Int -> Envi -> Bool
idExiste idEn (Envi contenu_envi) = M.foldlWithKey' idExiste False contenu_envi where
  idExiste :: Bool -> Coord -> [Entite] -> Bool
  idExiste True _ _ = True
  idExiste _ _ [] = False
  idExiste False coord (e:es) = if idEn == (entite_get_id e) then True
                                          else idExiste False coord es

instance Show Envi where
  show (Envi contenu_envi) = M.foldlWithKey' foldShowAux "" contenu_envi where
    foldShowAux :: String -> Coord -> [Entite] -> String
    foldShowAux str _ [] = str
    foldShowAux str coord (e:es) = str ++ (show e) ++ "," ++ (show coord) ++ "\n"