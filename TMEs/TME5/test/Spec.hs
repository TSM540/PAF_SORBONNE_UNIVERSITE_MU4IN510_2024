import Test.Hspec
import GFunSpec as GS

main :: IO ()
main = hspec $ do
  GS.engineSpec
