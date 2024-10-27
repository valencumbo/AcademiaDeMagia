module Spec where
  
import PdePreludat
import Library
import Academia
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
    it "Mago afectado por curacion" $ do
      lagrimaFenix 50 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 150, hechizos = [lagrima]}
      lagrimaFenix 0 potter `shouldBe` potter






