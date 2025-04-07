import Test.Hspec

import CarteTest as CT
import EnviTest as ET
import ModeleTest as MT


main :: IO ()
main = hspec $ do
    CT.invTest
    ET.methodsTest
    MT.modeleTest
