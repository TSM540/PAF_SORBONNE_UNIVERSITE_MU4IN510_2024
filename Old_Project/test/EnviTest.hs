module EnviTest where

import Test.Hspec

import Carte
import Environnement
import qualified Data.Map.Strict as M

methodsTest = do 
    describe "Envi:methods" $ do
        it "trouve_id" $ do
            let x = (Envi (M.fromList []))
            let env = ajout_entite (Joueur 1 10) (C 1 1) x
            (show $ trouve_id 1 env) `shouldBe` "Just (C {cx = 1, cy = 1},Joueur {idEn = 1, pv = 10})"

        it "bouge_id" $ do
            let x = (Envi (M.fromList []))
            let env = ajout_entite (Joueur 1 10) (C 1 1) x
            let env' = bouge_id 1 (C 2 5) env
            (show $ trouve_id 1 env') `shouldBe` "Just (C {cx = 2, cy = 5},Joueur {idEn = 1, pv = 10})"

        it "franchissable" $ do
            let x = (Envi (M.fromList []))
            let env = ajout_entite (Joueur 1 10) (C 1 1) x
            (show $ franchissable_env (C 1 1) env) `shouldBe` "False"

        it "rm_env_id" $ do
            let x = (Envi (M.fromList []))
            let env = ajout_entite (Joueur 1 10) (C 1 1) x
            let env' = ajout_entite (Vache 2 20) (C 3 2) env
            let env'' = rm_env_id 1 env'
            (show env'') `shouldBe` "Vache {idEn = 2, pv = 20},C {cx = 3, cy = 2}\n"
