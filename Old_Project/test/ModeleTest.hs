module ModeleTest where

import Test.Hspec

import Carte
import Environnement
import Modele

import qualified Data.Map.Strict as M
import System.Random as R
import Data.Set (Set)
import qualified Data.Set as S

modeleTest = do 
    describe "prevoit vache" $ do
        it "teste ordre vache 1, avec murs" $ do
            let x = (Envi (M.fromList []))
            let j1 = (Joueur 1 10)
            let vache = (Vache 2 20)
            let env = ajout_entite j1 (C 2 2) x
            let env' = ajout_entite vache (C 1 1) env
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            gen <- R.getStdGen
            let keyboard = S.fromList []
            let modele = (Cont carte env' gen "" keyboard)
            let listeOrdre = prevoit_vache modele vache
            (show listeOrdre) `shouldBe` "[(2,R),(1,E),(1,S)]"

        it "teste ordre vache 2, avec murs et le joueur" $ do
            let x = (Envi (M.fromList []))
            let j1 = (Joueur 1 10)
            let vache = (Vache 2 20)
            let env = ajout_entite j1 (C 2 2) x
            let env' = ajout_entite vache (C 1 2) env
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            gen <- R.getStdGen
            let keyboard = S.fromList []
            let modele = (Cont carte env' gen "" keyboard)
            let listeOrdre = prevoit_vache modele vache
            (show listeOrdre) `shouldBe` "[(2,R),(1,N)]"

        it "teste ordre vache 2, avec murs, le joueur et une porte" $ do
            let x = (Envi (M.fromList []))
            let j1 = (Joueur 1 10)
            let vache = (Vache 2 20)
            let env = ajout_entite j1 (C 4 2) x
            let env' = ajout_entite vache (C 4 1) env
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            gen <- R.getStdGen
            let keyboard = S.fromList []
            let modele = (Cont carte env' gen "" keyboard)
            let listeOrdre = prevoit_vache modele vache
            (show listeOrdre) `shouldBe` "[(2,R),(1,O)]"

    {- describe "Modele:decide" $ do
        it "teste pour voir si la selection aleatoire marche, obsolete" $ do
            let x = (Envi (M.fromList []))
            let j1 = (Joueur 1 10)
            let env = ajout_entite j1 (C 1 1) x
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            gen <- R.getStdGen
            let keyboard = S.fromList []
            let modele = (Cont carte env gen "" keyboard)
            let listeAction = [(2,N),(4,S),(1,E),(1,O)]
            let modele' = decide listeAction modele j1
            (show $ modele') `shouldBe` "Just (C {cx = 1, cy = 1},Joueur {idEn = 1, pv = 10})" -}

