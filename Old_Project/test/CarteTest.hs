module CarteTest where

import Test.Hspec

import Carte

invTest = do 
    describe "Carte:inv" $ do
        it "invariant correct" $ do
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "True"

        it "invariant incorrect -> Taille" $ do
            x <- readFile "cartes/carteWrongSize.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"

        it "invariant incorrect -> Murs" $ do
            x <- readFile "cartes/carteWrongWalls.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"

        it "invariant incorrect -> Entree" $ do
            x <- readFile "cartes/carteWrongEntree.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"

        it "invariant incorrect -> Sortie" $ do
            x <- readFile "cartes/carteWrongSortie.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"

        it "invariant incorrect -> Portes" $ do
            x <- readFile "cartes/carteWrongPortes.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"

        {- it "solvable correct" $ do
            x <- readFile "cartes/carte.txt"
            let carte = loadCarte x
            (show (isSolvable carte)) `shouldBe` "True" -}

        it "invariant incorrect -> Sortie inaccessible" $ do
            x <- readFile "cartes/carteUnsolvable.txt"
            let carte = read x :: Carte
            (show (carte_inv carte)) `shouldBe` "False"


editTest = undefined {-do
    describe "edit:Test" $ do
        it "edit correct" $ do-}