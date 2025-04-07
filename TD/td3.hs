import Data.Text (Text)
import Data.Text as Text 
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)


import qualified Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
import qualified Data.Sequence as Seq

-- ! outils
-- : map
map_pourtout :: (a -> Bool) -> Map k a -> Bool
map_pourtout predicat m = Map.fold aux m True
         where aux v f = f && (predicat v)

-- : Seq
seq_pourtout :: (a -> Bool) -> Seq a -> Bool
seq_pourtout predicat sequence = fold aux sequence True 
    where aux e f = f && (predicat e)
-- ! Systeme
data Systeme = Systeme {
    membres :: Map MemeberId Member,
    projets :: Map ProjectId Project
}

-- ** Invariant systeme
prop_SystemeInv :: Systeme -> Bool
prop_SystemeInv (Systeme {membres = m, projets = p}) = 
    map_pourtout prop_MemberInv m

-- constructeur
initSysteme :: ()-> Systeme
initSysteme = Systeme Map.empty Map.empty




-- Prop constructeur
prop_PostInitSysteme :: ()->Bool
prop_PostInitSysteme () = prop_SystemeInv (initSysteme())

-- !s Membre 
newtype MemeberId = MemeberId Integer
    deriving (Eq, Ord, Show)

newtype LoginInfo = LoginInfo Text
newtype AdresseMail = AdresseMail Text

-- invariant mail 

prop_AdresseMailInv :: AdresseMail -> Bool
prop_AdresseMailInv (AdresseMail a) = Text.isInfixOf "@" a
data Member = Member
    { nom :: Text
    , prenom :: Text
    , login :: LoginInfo
    , mele :: AdresseMail
    }
-- inv des membres 
prop_MemberInv :: Member -> Bool
prop_MemberInv Member{prenom = p, nom = n, login = (LoginInfo l), mele = (AdresseMail a)} =
            -- p /= Text.empty && n /= Text.empty && l /= Text.empty && a /= Text.empty
            not(Text.empty p) && not(Text.empty n) && not(Text.empty l) && not(Text.empty a) && prop_AdresseMailInv (AdresseMail a) 

            

-- data Project ={
--     nom :: Text,
--     prenom :: Text,
--     login :: LoginInfo,
--     mele :: AdresseMail
-- }
-- constructeur 

initMember :: Text -> Text -> Text -> Text -> Maybe Member
initMember n p l m 
    | not (Text.empty p) && not (Text.empty n) && not (Text.empty l) && not (Text.empty a) && prop_AdresseMailInv (AdresseMail a) = Just $ Member n p (LoginInfo l) (AdresseMail m)
    | otherwise = Nothing

-- ! Projet
newtype ProjectId = ProjectId Integer
data Project = Project {
    nomComplet :: Text,
    equipe :: Set MemeberId,
    tableaux :: Map TabId TabInfo
}
-- invariant projet
-- constructeur

-- ! Tableaux 
newtype TabId = TabId Text

data TabInfo = Tab {
    cartes :: Seq Carte
}
-- invariant tableaux
-- constructeur

-- Cartes
data Carte = Carte {
    nomCarte :: Text,
    descriptionCarte :: Text,
    membresCarte :: Seq MemeberId,
    taches :: Seq TacheInfo
}

-- invariant carte
prop_CarteInvText :: Carte -> Bool
prop_CarteInvText Carte{nomCarte = n, descriptionCarte = d} = 
    not(Text.empty n) && not(Text.empty d)

-- prop_CarteInvMembre :: Carte -> Bool
-- prop_CarteInvMembre Carte{membresCarte = m} = 
--     fold aux True m 
--     where aux v f = f && (Set.member v (membres s))
prop_CarteInvTache :: Carte -> Bool
prop_CarteInvTache Carte{taches = t } =
    fold aux True t
    where aux ta f = f && (prop_TacheInv t)

prop_CarteInvMembre :: Tache -> Bool
prop_CarteInvMembre Carte{carteMembres = m, taches = t } = 
    seq_pourtout aux t 
    where aux ta = (tachesMembres ta) `Set.isSubsetOf` m

prop_CarteInv :: Carte -> Bool
prop_CarteInv c = (prop_CarteInvText c) && (prop_CarteInvTache c)

-- constructeur

-- !Taches
data TacheInfo = Tache {
    descriptionTache :: Text,
    tacheDate :: Date
}

data Tache =  TacheLibre TacheInfo 
            | TacheEnCours TacheInfo (Set MemeberId) TacheDeadLine
            | TacheFinie TacheInfo (Set MemeberId) Date
data TacheDeadLine = Illimite
                    | Deadline Date 
data Date = DateAbstraite deriving(Show,Eq,Ord)

-- obserevateur
tacheMembres :: Tache -> Set MemeberId
tacheMembres (TacheLibre _) = Set.empty
tacheMembres (TacheEnCours _ m _) = m
tacheMembres (TacheFinie _ m _) = m

-- invariant 
-- constructeur
-- operations
-- ! assigenement : preconditions
prop_tacheAssignePre :: Tache -> Set MemeberId -> TacheDeadLine -> Bool
prop_tacheAssignePre (TacheLibre _) mes _ = not(Set.null mes)
prop_tacheAssignePre _ _ _ = False
-- ! assigner une tache
tacheAssigne :: Tache -> Set MemeberId -> TacheDeadline -> Tache
tacheAssigne (TacheLibre tinfo) ms dl = TacheEnCours tinfo ms dl 
tacheAssigne _ _ _ = error "tache deja assigne ou finie"
-- ! assigenement : postconditions 
prop_tacheAssignePost :: Tache -> Set MemeberId -> TacheDeadLine -> Bool
prop_tacheAssignePost ta mes dl = case tacheAssigne ta mes of
            (TacheEnCours ti tmes tdl) -> tmes == mes && dl == tdl -- et que l'info de la tache est la meme
            | _ -> False
      