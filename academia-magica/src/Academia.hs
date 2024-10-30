module Academia where

import PdePreludat

data Mago = Mago{
    nombre :: String,
    edad :: Number,
    salud :: Number,
    hechizos :: [Hechizo]
} deriving (Show)

instance Eq Mago where
    (Mago nombre edad salud hechizos) == (Mago otroNombre otraEdad otraSalud otroHechizos) =
        nombre == otroNombre && edad == otraEdad && salud == otraSalud && hechizos == otroHechizos

data Hechizo = Hechizo{
    nombreHechizo :: String,
    efecto :: EfectoHechizo
} deriving (Show)

instance Eq Hechizo where
    (Hechizo nombreHechizo _) == (Hechizo otroNombreHechizo _) =
        nombreHechizo == otroNombreHechizo

type EfectoHechizo = Mago -> Mago

lagrimaFenix :: Number -> EfectoHechizo
lagrimaFenix curacion mago = mago{salud = salud mago + curacion}

sectumSempra :: EfectoHechizo
sectumSempra mago = mago{salud = salud mago - efectoSectum (salud mago)}

efectoSectum :: Number -> Number
efectoSectum salud | salud > 10 = 10
                   | otherwise  = div salud 2

obliviate :: Number -> EfectoHechizo
obliviate olvidados mago = mago{hechizos = drop olvidados (hechizos mago)}

confundus :: EfectoHechizo
confundus mago | null (hechizos mago) || nombreHechizo primerHechizo  == "confundus" = mago
               | otherwise = efecto primerHechizo mago
            where primerHechizo = head (hechizos mago)

poder :: Mago -> Number
poder mago = salud mago + (edad mago * length (hechizos mago))

daño :: Mago -> Hechizo -> Number
daño mago hechizo | saludMagoHechizado < salud mago = -(salud mago - saludMagoHechizado)
                  | otherwise = 0
            where saludMagoHechizado = salud (efecto hechizo mago)

diferenciaDePoder :: Mago -> Mago -> Number
diferenciaDePoder mago otroMago = abs (poder mago - poder otroMago)

type AcademiaMagica = [Mago]

hogwarts :: AcademiaMagica
hogwarts = [potter, weasley, malfoy, goyle, granger, snape, dumbledore, crabbe, rubeus]

koldovstoretz :: AcademiaMagica
koldovstoretz = [bazarov, ibrahimova]

beauxbatons :: AcademiaMagica
beauxbatons = []

sinHechizosHadrid :: AcademiaMagica -> Bool
sinHechizosHadrid [] = False
sinHechizosHadrid(x:xs) | nombre x == "Hadrid" && null (hechizos x) = True
                        | otherwise = sinHechizosHadrid xs

potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
weasley = Mago{nombre = "Ron", edad = 21, salud = 5, hechizos = [lagrima, sectum]}
malfoy = Mago{nombre = "Draco", edad = 22, salud = 10, hechizos = [lagrima, sectum]}
goyle = Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
granger = Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [lagrima, sectum, obliviateHechizo]}
snape = Mago{nombre = "Severus", edad = 50, salud = 200, hechizos = [sectum, obliviateHechizo, lagrima, confundusHechizo]}
dumbledore = Mago{nombre = "Albus", edad = 50, salud = 350, hechizos = [confundusHechizo, obliviateHechizo, lagrima, sectum]}
crabbe = Mago{nombre = "Vincent", edad = 9, salud = 6, hechizos = []}
rubeus = Mago{nombre = "Hagrid", edad = 40, salud = 200, hechizos = [confundusHechizo, obliviateHechizo, lagrima, sectum]}
bazarov = Mago{nombre = "Spartacus", edad = 45, salud = 120, hechizos = [confundusHechizo, lagrima, sectum]}  
ibrahimova = Mago{nombre = "Stella", edad = 50, salud = 140, hechizos = [lagrima, sectum]}  
 
lagrima = Hechizo{nombreHechizo = "lagrimaFenix", efecto = lagrimaFenix 30}
sectum = Hechizo{nombreHechizo = "sectumSempra", efecto = sectumSempra}
obliviateHechizo = Hechizo{nombreHechizo = "obliviate", efecto = obliviate 2}
confundusHechizo = Hechizo{nombreHechizo = "confundus", efecto = confundus}
