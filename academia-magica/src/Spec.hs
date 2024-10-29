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
      lagrimaFenix 0 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
    it "Mago afectado por sectum" $ do
      sectumSempra potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 90, hechizos = [lagrima]}
      -- sectumSempra weasley `shouldBe` Mago{nombre = "Ron", edad = 21 , salud = 2.5, hechizos = [lagrima, sectum]}
      sectumSempra malfoy `shouldBe` Mago{nombre = "Draco", edad = 22 , salud = 5, hechizos = [lagrima, sectum]}
    it "Mago afectado por obliviate" $ do
      obliviate 1 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = []}
      obliviate 2 granger `shouldBe` Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [obliviateHechizo]}
      obliviate 10 goyle `shouldBe` Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
    it "Mago afectado por confundus" $ do
      confundus potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 130, hechizos = [lagrima]}
      confundus snape `shouldBe` Mago{nombre = "Severus", edad = 50, salud = 190, hechizos = [sectum, obliviateHechizo, lagrima, confundusHechizo]}



