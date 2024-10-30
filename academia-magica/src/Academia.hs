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

poder :: Mago -> Int
poder mago = salud mago + ((edad mago) * length(hechizos mago))

daño :: Mago -> Hechizo -> Int
daño mago hechizo | nombreHechizo hechizo == "lagrimaFenix"  = -(valor hechizo)
                  | nombreHechizo hechizo == "obliviate" = 0
                  | otherwise = valor hechizo

diferenciaDePoder :: Mago -> Mago -> Int
diferenciaDePoder mago otroMago = abs(poder mago - poder otroMago)

type Academia = [Mago]

hogwarts = [potter , weasley , hadrid]

sinHechizosHadrid :: Academia -> Bool
sinHechizosHadrid [] = False
sinHechizosHadrid(mago:magos) | nombre mago == "Hadrid" && null (hechizos mago) = True
                        | otherwise = sinHechizosHadrid magos

viejosNonos :: Academia -> Bool
viejosNonos[] = True
viejosNonos(mago:magos)  = (edad mago > 16) && esNono mago && viejosNonos magos      
                  
esNono :: Mago -> Bool
esNono mago = length (hechizos mago) > 3 * edad mago

{-
    Analizar la siguiente funcion:

    f x [y] = y
    f x (y1:y2:ys)
        | x y1 >= x y2 = f x (y1:ys)
        | otherwise = f x (y2 : ys)

    Se trata de una funcion que recibe una lista y devuelve su maximo.
    En el caso base con un solo elemento devuelve dicho elemento.
    Si no va comparando los 2 primeros elementos de la lista,descarta el menor y de manera recursiva
    recorre la lista hasta el final.

 -}

---Mejoro expresividad
obtenerMaximo :: Ord a1 => (a2 -> a1) -> [a2] -> a2
obtenerMaximo lista [elemento] = elemento
obtenerMaximo lista (primerElemento:segundoElemento:cola)| lista primerElemento >= lista segundoElemento = obtenerMaximo lista (primerElemento:cola)
                                                         | otherwise = obtenerMaximo lista (segundoElemento : cola)

mejorOponente :: Mago -> Academia -> Mago
mejorOponente mago  = obtenerMaximo (\oponente -> abs (poder oponente - poder mago))

potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
weasley = Mago{nombre = "Ron", edad = 21, salud = 5, hechizos = [lagrima, sectum]}
malfoy = Mago{nombre = "Draco", edad = 22, salud = 10, hechizos = [lagrima, sectum]}
goyle = Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
granger = Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [lagrima, sectum, obliviateHechizo]}

lagrima = Hechizo{nombreHechizo = "lagrimaFenix", efecto = lagrimaFenix 30}
sectum = Hechizo{nombreHechizo = "sectumSempra", efecto = sectumSempra}
obliviateHechizo = Hechizo{nombreHechizo = "obliviate", efecto = obliviate 2}
