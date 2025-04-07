module Carte where

import qualified Data.Map.Strict as M

-- direction d'une porte, NORD/SUD ou EST/OUEST
data PorteDirection = NS | EO deriving (Eq)

-- statut d'une porte, ouverte ou fermée
data StatutPorte = Ouverte | Fermee deriving (Eq)

-- Contenu possible d'une case
data Case = Normal
   | Porte PorteDirection StatutPorte
   | Mur
   | Entree
   | Sortie
   | Other
   deriving (Eq)

data Coord = C {cx :: Int, cy :: Int} deriving (Eq, Show)

-- ordre de la carte, début au nord-ouest, fin sud-est
instance Ord Coord where
  (C cx1 cy1) <= (C cx2 cy2) 
    | cy1 < cy2 = True
    | cy1 > cy2 = False
    | cy1 == cy2 = cx1 < cx2
    

data Carte = Carte {cartel :: Int, -- largeur
                    carteh :: Int, -- hauteur
                    carteContenu :: (M.Map Coord Case)} -- contenu de la carte


-- invariant de la carte, doit etre valide
carte_inv :: Carte -> Bool
carte_inv c = 
  (casesDansRectangle c) && (casesExistent c) && (uniqueEntree c) && (uniqueSortie c) &&
  (entoureDeMur c) && (portesEncadres c) && (isSolvable c)


-- verifie que la sortie est accesible depuis l'entree
isSolvable :: Carte -> Bool
isSolvable carte = isSolvableAux carte (getEntree carte) where
  isSolvableAux :: Carte -> Coord -> Bool
  isSolvableAux carte@(Carte l h contenu) coord@(C x y) 
    | c == Normal || c == Porte NS Ouverte || c == Porte NS Fermee 
      || c == Porte EO Ouverte || c == Porte EO Fermee || c == Entree = 
        isSolvableAux (editCase carte coord Mur) (C (x+1) y) || isSolvableAux (editCase carte coord Mur) (C (x-1) y)
        || isSolvableAux (editCase carte coord Mur) (C x (y+1)) || isSolvableAux (editCase carte coord Mur) (C x (y-1))
    | c == Sortie = True
    | otherwise = False
    where
      c = getCaseContent carte coord

instance Show Carte where
    show (Carte l h content) = 
      cartePrinter (foldl foldCarteAux "" content) l

charToCase :: Char -> Case
charToCase c
  | c == ' ' = Normal
  | c == '-' = (Porte NS Fermee)
  | c == '|' = (Porte EO Fermee)
  | c == 'X' = Mur
  | c == 'E' = Entree
  | c == 'S' = Sortie
  | otherwise = Other

-- verifie que les portes sont entre 2 murs
portesEncadres :: Carte -> Bool
portesEncadres (Carte l h contenu) = portesEncadresAux contenu ([(i,j) | i <- [0..(l-1)], j <- [0..(h-1)]]) where
  portesEncadresAux :: M.Map Coord Case -> [(Int,Int)] -> Bool
  portesEncadresAux contenu [] = True
  portesEncadresAux contenu (x:xs) = case M.lookup (C (fst x) (snd x)) contenu of
    Just c  -- on verifie que la case courrante est une porte, puis que ses cases adjacentes sont des murs
      | c == (Porte NS Fermee) -> case M.lookup (C ((fst x) + 1) (snd x)) contenu of
        Just c2
          | c2 == Mur -> case M.lookup (C ((fst x) - 1) (snd x)) contenu of
            Just c3
              | c3 == Mur -> portesEncadresAux contenu xs
              | otherwise -> False
            Nothing -> error "case non existante ?? portesEncadresAux"
          | otherwise -> False
        Nothing -> error "case non existante ?? portesEncadresAux"
      | c == (Porte EO Fermee) -> case M.lookup (C (fst x) ((snd x) + 1)) contenu of
        Just c2
          | c2 == Mur -> case M.lookup (C (fst x) ((snd x) - 1)) contenu of
            Just c3
              | c3 == Mur -> portesEncadresAux contenu xs
              | otherwise -> False
            Nothing -> error "case non existante ?? portesEncadresAux"
          | otherwise -> False
        Nothing -> error "case non existante ?? portesEncadresAux"
      | c == (Porte NS Ouverte) -> case M.lookup (C ((fst x) + 1) (snd x)) contenu of
        Just c2
          | c2 == Mur -> case M.lookup (C ((fst x) - 1) (snd x)) contenu of
            Just c3
              | c3 == Mur -> portesEncadresAux contenu xs
              | otherwise -> False
            Nothing -> error "case non existante ?? portesEncadresAux"
          | otherwise -> False
        Nothing -> error "case non existante ?? portesEncadresAux"
      | c == (Porte EO Ouverte) -> case M.lookup (C (fst x) ((snd x) + 1)) contenu of
        Just c2
          | c2 == Mur -> case M.lookup (C (fst x) ((snd x) - 1)) contenu of
            Just c3
              | c3 == Mur -> portesEncadresAux contenu xs
              | otherwise -> False
            Nothing -> error "case non existante ?? portesEncadresAux"
          | otherwise -> False
        Nothing -> error "case non existante ?? portesEncadresAux"
      | otherwise -> portesEncadresAux contenu xs
    Nothing -> error "case non existante ?? portesEncadresAux"


-- verifie que la carte est entouree de murs
-- cad toutes les cases avec x=0, y=0, x=largeur, y=hauteur
entoureDeMur :: Carte -> Bool
entoureDeMur carte@(Carte l h contenu) = entoureDeMurAux contenu (((,) <$> [0,(l-1)] <*> [0..(h-1)]) ++ ((,) <$> [1..(l-2)] <*> [0,(h-1)])) where
  entoureDeMurAux :: M.Map Coord Case -> [(Int, Int)] -> Bool
  entoureDeMurAux contenu [] = True
  entoureDeMurAux contenu (x:xs) = 
    if (getCaseContent carte (C (fst x) (snd x))) == Mur then entoureDeMurAux contenu xs else False


-- verifie que toutes les cases du rectangle ont une valeur dans la map
casesExistent :: Carte -> Bool
casesExistent (Carte l h contenu) = casesExistentAux contenu ([(i,j) | i <- [0..(l-1)], j <- [0..(h-1)]]) where
  casesExistentAux :: M.Map Coord Case -> [(Int,Int)] -> Bool
  casesExistentAux contenu [] = True
  casesExistentAux contenu (x:xs) = case M.lookup (C (fst x) (snd x)) contenu of
    Just c -> casesExistentAux contenu xs
    Nothing -> False


-- verifie que les cases sont toutes contenues dans le rectangle defini par la hauteur et la largeur
casesDansRectangle :: Carte -> Bool
casesDansRectangle (Carte l h contenu) = casesDansRectangleAux $ M.keys contenu where
  casesDansRectangleAux :: [Coord] -> Bool
  casesDansRectangleAux [] = True
  casesDansRectangleAux ((C x y):xs) = if x < l && y < h && x >= 0 && y >= 0 then casesDansRectangleAux xs else False


-- verifie qu'il n'y a qu'une unique sortie dans la carte
uniqueSortie :: Carte -> Bool
uniqueSortie carte@(Carte l h contenu) = uniqueSortieAux contenu ([(i,j) | i <- [0..(l-1)], j <- [0..(h-1)]]) 0 where
  uniqueSortieAux :: M.Map Coord Case -> [(Int,Int)] -> Int -> Bool
  uniqueSortieAux contenu [] i = if i == 1 then True else False
  uniqueSortieAux contenu (x:xs) i =
    if (getCaseContent carte (C (fst x) (snd x))) == Sortie then uniqueSortieAux contenu xs (i+1) else uniqueSortieAux contenu xs i


-- verifie qu'il n'y a qu'une unique entree dans la carte
uniqueEntree :: Carte -> Bool
uniqueEntree carte@(Carte l h contenu) = uniqueEntreeAux contenu ([(i,j) | i <- [0..(l-1)], j <- [0..(h-1)]]) 0 where
  uniqueEntreeAux :: M.Map Coord Case -> [(Int,Int)] -> Int -> Bool
  uniqueEntreeAux contenu [] i = if i == 1 then True else False
  uniqueEntreeAux contenu (x:xs) i = 
    if (getCaseContent carte (C (fst x) (snd x))) == Entree then uniqueEntreeAux contenu xs (i+1) else uniqueEntreeAux contenu xs i


-- recupere les coordonnes de l'entree
getEntree :: Carte -> Coord
getEntree carte@(Carte l h contenu) = getEntreeAux contenu ([(i,j) | i <- [0..(l-1)], j <- [0..(h-1)]]) where
  getEntreeAux :: M.Map Coord Case -> [(Int,Int)] -> Coord
  getEntreeAux contenu [] = error "pas d'entree"
  getEntreeAux contenu (x:xs) = 
    if (getCaseContent carte (C (fst x) (snd x))) == Entree then (C (fst x) (snd x)) else getEntreeAux contenu xs

-- recupere le contenu d'une case selon ses coordonees
getCaseContent :: Carte -> Coord -> Case
getCaseContent (Carte l h contenu) coord = case M.lookup coord contenu of
  Just c -> c
  Nothing -> error "case non existante ??"


-- verifie si une case est une porte ou un mur
-- (et donc infranchissable)
isPorteOuMur :: Carte -> Coord -> Bool
isPorteOuMur carte coord = if c == (Porte NS Ouverte) || c == (Porte NS Fermee) || c == (Porte EO Ouverte)
              || c == (Porte EO Fermee) || c == Mur then True else False
    where
      c = getCaseContent carte coord


-- precondition avant de modifier une case,
-- on verifie que la carte est valide
editCase_pre :: Carte -> Bool
editCase_pre c = carte_inv c 

-- modifie une case de la carte
editCase :: Carte -> Coord -> Case -> Carte
editCase carte@(Carte l h contenu) coord c =  (Carte l h $ M.insert coord c contenu)

--postcondition apres la modification d'une case,
-- on verifie que les proprietes propres au contenu
-- des cases sont encore valides
editCase_post :: Carte -> Bool
editCase_post c = (uniqueEntree c) && (uniqueSortie c) && (entoureDeMur c) && (portesEncadres c)


-- precondition avant d'ouvrir une porte,
-- on verifie que la carte est valide
-- et que la case est une porte
modifierPorte_pre :: Carte -> Case -> Bool
modifierPorte_pre carte c = carte_inv carte && (c == (Porte NS Fermee) ||  c == (Porte EO Fermee) ||
                             c == (Porte NS Ouverte) || c == (Porte EO Ouverte))

-- ouverture et fermeture d'une porte
modifierPorte :: Carte -> Coord -> Carte
modifierPorte carte@(Carte l h contenu) coord 
  | c == (Porte NS Fermee) = editCase carte coord (Porte NS Ouverte)
  | c == (Porte EO Fermee) = editCase carte coord (Porte EO Ouverte)
  | c == (Porte NS Ouverte) = editCase carte coord (Porte NS Fermee)
  | c == (Porte EO Ouverte) = editCase carte coord (Porte EO Fermee)
  | otherwise = error "mauvais input"
  where
    c = getCaseContent carte coord

-- postcondition apres modification d'une porte,
-- verifie que la carte est toujours valide apres modification
modifierPorte_post :: Carte -> Bool
modifierPorte_post c = carte_inv c


-- fonction qui gère l'affichage, on reste à qqch de simple pour le moment
foldCarteAux :: String -> Case -> String
foldCarteAux s c 
  | c == Normal = s ++ " "
  | c == (Porte NS Ouverte) = s ++ " "
  | c == (Porte EO Ouverte) = s ++ " "
  | c == (Porte NS Fermee) = s ++ "-"
  | c == (Porte EO Fermee) = s ++ "|"
  | c == Mur = s ++ "X"
  | c == Entree = s ++ "E"
  | c == Sortie = s ++ "S"
  | c == Other = s ++ ""


-- carte complète avec \n selon la largeur
cartePrinter :: String -> Int -> String
cartePrinter s l = cartePrinterAux s l 0 "" where
  cartePrinterAux :: String -> Int -> Int -> String -> String
  cartePrinterAux s l i res 
    | i >= (length s) = res
    | i `mod` l == 0 && (length res) /= 0 = cartePrinterAux s l (i+1) (res ++ "\n" ++ [s!!i])
    | otherwise = cartePrinterAux s l (i+1) (res ++ [s!!i])




-- charge une carte a partir d'une chaine de caractere (fichier)
loadCarte :: String -> Carte
loadCarte s = readMapInput 0 0 0 0 s M.empty where
      readMapInput :: Int -> Int -> Int -> Int -> String -> M.Map Coord Case -> Carte
      readMapInput x y i l s map -- largeur, hauteur, index, largeur conserve, carte, map de la carte
        | length s <= i = (Carte l y map)
        | s!!i == '\n' = readMapInput 0 (y+1) (i+1) x s map
        | otherwise = readMapInput (x+1) y (i+1) l s (M.insert (C x y) (charToCase (s!!i)) map)

-- Exception : no parse, use loadCarte instead
instance Read Carte where
    readsPrec _ s = [((loadCarte s), "")]
      
      {- readMapInput 0 0 0 0 s M.empty where
      readMapInput :: Int -> Int -> Int -> Int -> String -> M.Map Coord Case -> [(Carte,String)]
      readMapInput x y i l s map -- largeur, hauteur, index, largeur conserve, carte, map de la carte
        | length s <= i = [((Carte l y map),"")]
        | s!!i == '\n' = readMapInput 0 (y+1) (i+1) x s map
        | otherwise = readMapInput (x+1) y (i+1) l s (M.insert (C x y) (charToCase (s!!i)) map) -}
