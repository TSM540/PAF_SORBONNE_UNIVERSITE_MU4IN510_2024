data Zone = Zone Integer Integer -- h l
data Coord = Coord Integer Integer -- x y
-- coordonées (l,0) BG -> (l,h) HD
data Direction = H | B | G | D
data Mouvement = Mouv Direction Integer 
   deriving (Eq)
    -- Mouvement direction et distance
-- Parrtie 1
bougeCoord :: Coord -> Mouvement -> Coord
bougeCoord c@(Coord x y) m@(Mouv  direction distance) 
-- syntaxe error don't know how to solve it
    case direction of 
        |  H = Coord x (y+distance)
        |  B = Coord x (y-distance)
        |  G = Coord (x-distance) y
        |  D = Coord (x+distance) y


bougeCoordSafe :: Coord -> Mouvement -> Zone -> Maybe Coord
bougeCoordSafe c@(Coord x y) m@(Mouv direction distance) z@(Zone h l) =
     let c' = bougeCoord c m in
        case c' of 
            (Coord x' y') -> 
                if (x' >= -h && x' <= h && y' >= 0 && y' <= l) 
                    then Just c' 
                    else Nothing 
prop_gaucheDroite_bougCoord :: Coord -> Integer -> Bool
prop_gaucheDroite_bougCoord c@(Coord x y) d = 
   bougeCoord (bougCoord (Mouv G d)) (Mouv D d) == c

-- par induction
-- Non on ne peut pas (y a un Maybe je pense 😂)

-- Partie 2
import Data.Sequence as Seq
data HitBox = Rect Coord Integer Integer 
     | Composite (Seq HitBox)

prop_inv_hitbox :: HitBox -> Bool
prop_inv_hitbox (Rect c h l) = h > 0 && l > 0

type ConstructeurHitbox = [(Coord, Coord)] -> HitBox
appartient :: Coord -> HitBox -> Bool
appartient c@(Coord cx cy) hb(Rect (Coord x y) h l) = 
    cx >= x && cx <= x+l && cy >= y && cy <= y+h

bougeHitbox :: Hitbox -> Mouvement -> HitBox
bougeHitbox (Rect c h l) m = Rect (bougeCoord c m) h l

post_cond_bougeHitbox :: HitBox -> Mouvement -> HitBox -> Bool
post_cond_bougeHitbox hb m hb' = 
    forall c. appartient c hb' == appartient (bougeCoord c m) hb

bougeHitboxSafe :: HitBox -> Mouvement -> Zone -> Maybe HitBox
bougeHitboxSafe hb m z = 
    let hb' = bougeHitbox hb m in
        if forall c. appartient c hb' 
            then Just hb' 
            else Nothing
collision :: Hitbox -> Hitbox -> Bool
collision hb1 hb2 = 
    exists c. appartient c hb1 && appartient c hb2

-- Partie 3
data EtatCombattant =   Ko
                      | Ok Integer -- sa sante actuelle

data Combattant = Comb {
    positionc :: Coord ,
    hitboxc :: Hitbox ,
    facec :: Direction ,
    etatc :: EtatCombattant
}
data Jeu = GameOver Integer -- le numero du joueur vainqueur
          | EnCours 
        {
            joueur1 :: Combattant ,
            joueur2 :: Combattant ,
            zoneJeu :: Zone
        }
prop_inv_pos_combatants_dans_zone :: Jeu -> Bool
prop_inv_pos_combatants_dans_zone (EnCours c1 c2 z) = 
    appartient (positionc c1) (hitboxc c1) && appartient (positionc c2) (hitboxc c2) && 
    appartient (positionc c1) z && appartient (positionc c2) z
prop_inv_hitbox_combattants_chevauchent_pas:: Jeu -> Bool
prop_inv_hitbox_combattants_chevauchent_pas (EnCours c1 c2 z) = 
    not (collision (hitboxc c1) (hitboxc c2))
prop_inv_combattants_se_font_face :: Jeu -> Bool
prop_inv_combattants_se_font_face (EnCours c1 c2 z) = 
    facec c1 /= facec c2

tourJeu :: Jeu -> Jeu
tourJeu j@(EnCours c1 c2 z) 
    |   etatc c1 == Ko = GameOver 2
    |   etatc c2 == Ko = GameOver 1
    |   (etatc c1 == Ok v )&& (v<= 0) = GameOver 2
    |   (etatc c2 == Ok v )&& (v<= 0) = GameOver 1
    |   otherwise = error "Jeu en cours"
bougeJouer :: Jeu -> Combattant -> Mouvement -> Jeu
bougeJouer j@(EnCours c1 c2 z) c m = 
    let c' = c {positionc = bougeCoord (positionc c) m} in
        if appartient (positionc c') z 
            then EnCours c' c2 z 
            else j
prop_post_cond_bougeJouer :: Jeu -> Combattant -> Mouvement -> Jeu -> Bool
prop_post_cond_bougeJouer j c m j' = 
    forall c. appartient (positionc c) (zoneJeu j') && appartient (positionc c) (hitboxc c) 
        && not (collision (hitboxc c) (hitboxc c2)) && facec c == facec c2

-- Partie 4 a faire en bonus