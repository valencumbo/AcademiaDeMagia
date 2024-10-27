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

potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
lagrima = Hechizo{nombreHechizo = "lagrimaFenix", efecto = lagrimaFenix 30}