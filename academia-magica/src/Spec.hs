module Spec where
  
import PdePreludat
import Library
import Academia
import Test.Hspec
import GHC.Windows (failIfFalse_)

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
      sectumSempra weasley `shouldBe` Mago{nombre = "Ron", edad = 21 , salud = 3, hechizos = [lagrima, sectum]} -- por tipo de dato Number, la division redondea hacia arriba
      sectumSempra malfoy `shouldBe` Mago{nombre = "Draco", edad = 22 , salud = 5, hechizos = [lagrima, sectum]}
    it "Mago afectado por obliviate" $ do
      obliviate 1 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = []}
      obliviate 2 granger `shouldBe` Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [obliviateHechizo]}
      obliviate 10 goyle `shouldBe` Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
    it "Mago afectado por confundus" $ do
      confundus potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 130, hechizos = [lagrima]}
      confundus snape `shouldBe` Mago{nombre = "Severus", edad = 50, salud = 190, hechizos = [sectum, obliviateHechizo, lagrima, confundusHechizo]}
      confundus dumbledore `shouldBe` Mago{nombre = "Albus", edad = 50, salud = 350, hechizos = [confundusHechizo, obliviateHechizo, lagrima, sectum]}
    it "Poder del Mago" $ do
      poder potter `shouldBe` 120
      poder dumbledore `shouldBe` 550
    it "Poder del Mago sin hechizos" $ do
      poder crabbe `shouldBe` 6
      poder goyle `shouldBe` 2
    it "Daño producido al mago" $ do
      daño granger sectum `shouldBe` -10
      daño crabbe sectum `shouldBe` -3
    it "Sin Daños al mago" $ do
      daño snape lagrima `shouldBe` 0
      daño dumbledore obliviateHechizo `shouldBe` 0
    it "Diferencia de poder entre magos" $ do
      diferenciDePoder potter weasley `shouldBe` 73
      diferenciDePoder snape dumbledore `shouldBe` 150
      diferenciDePoder granger malfoy `shouldBe` 79
    it "Mago sin hechizos" $ do
      magoSinHechizos "Harry" hogwarts `shouldBe` False
      magoSinHechizos "Spartacus" koldovstoretz `shouldBe` False
      magoSinHechizos "Vincent" hogwarts `shouldBe` True
    it "Hay algun mago llamado Hagrid sin hechizos" $ do
      magoSinHechizos "Hagrid" hogwarts `shouldBe` True
      magoSinHechizos "Hagrid" beauxbatons `shouldBe` False
    it "Todos los magos viejos son nionios" $ do 
      viejosNionios hogwarts `shouldBe` False 
      viejosNionios koldovstoretz `shouldBe` False
      viejosNionios beauxbatons `shouldBe` False
    it "Hechizo mas efectivo contra otro mago" $ do
      mejorHechizoContra potter malfoy `shouldBe` sectum
      mejorHechizoContra potter granger `shouldBe` sectum
      mejorHechizoContra weasley dumbledore `shouldBe` sectum
    it "Mejor oponente para un mago dentro de una academia" $do
      mejorOponente potter hogwarts `shouldBe` Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
      mejorOponente granger koldovstoretz `shouldBe` Mago {nombre = "Stella", edad = 50, salud = 140, hechizos = [lagrima, sectum]}
    it "Mago no puede ganar" $ do
      noPuedeGanarle crabbe potter `shouldBe` True
      noPuedeGanarle goyle snape `shouldBe` True
    it "Mago si puede ganar" $ do -- Casos donde noPuedeGanarle deberia dar ´False´
      noPuedeGanarle granger weasley `shouldBe` False
      noPuedeGanarle dumbledore snape `shouldBe` False

