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

potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
weasley = Mago{nombre = "Ron", edad = 21, salud = 5, hechizos = [lagrima, sectum]}
malfoy = Mago{nombre = "Draco", edad = 22, salud = 10, hechizos = [lagrima, sectum]}
goyle = Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
granger = Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [lagrima, sectum, obliviateHechizo]}

lagrima = Hechizo{nombreHechizo = "lagrimaFenix", efecto = lagrimaFenix 30}
sectum = Hechizo{nombreHechizo = "sectumSempra", efecto = sectumSempra}
obliviateHechizo = Hechizo{nombreHechizo = "obliviate", efecto = obliviate 2}