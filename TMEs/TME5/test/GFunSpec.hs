module GFunSpec where

import GFun

import Test.Hspec
import Control.Exception (evaluate)

minusOnes = Z (-1) minusOnes
ones = Z 1 ones
twos = Z 2 twos
threes = Z 3 threes
fours = Z 4 fours
fives = Z 5 fives



prendSpec = do
    describe "prendSpec" $ do

        it "returns a list of elements in a serie" $ do
            prend 10 twos `shouldBe` (take 10 (repeat 2))
            
        it "returns an empty list if the number is 0" $ do
            prend 0 twos `shouldBe` []

        it "returns an error if the number is negative" $ do
            evaluate (prend (-1) twos) `shouldThrow` errorCall "cannot take a negative number of element from serie"

fmapSpec = do
    describe "fmapSpec" $ do

        it "applies a function onto the element in a serie" $ do
            fmap (*2) twos `shouldBe` fours

instanceNumSpec = do
    describe "instanceNumSpec" $ do

        it "supports addition of 2 series" $ do
            twos + threes `shouldBe` fives

        it "supports substraction of 2 series" $ do
            threes - twos `shouldBe` ones

        it "supports multiplication of 2 series" $ do
            prend 10 (twos * twos) `shouldBe` (take 10 (iterate (+4) 4))

        it "supports giving the absolute values of serie" $ do
            abs (twos - threes) `shouldBe` ones

        it "supports giving signum of values of series" $ do
            signum (negate twos) `shouldBe` minusOnes

        it "supports generate a serie from an integer" $ do
            (fromInteger 4 :: Serie Int) `shouldBe` fours

engineSpec = do
    prendSpec
    fmapSpec
    instanceNumSpec
        

        

