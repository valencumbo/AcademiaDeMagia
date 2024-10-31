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

magoSinHechizos :: String -> AcademiaMagica -> Bool
magoSinHechizos nombreMago = any (\mago -> nombre mago == nombreMago && null(hechizos mago))

viejosNonos :: AcademiaMagica -> Bool
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

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra objetivo agresor = obtenerMaximo (\hechizo -> daño objetivo hechizo) (hechizos agresor)

mejorOponente :: Mago -> AcademiaMagica -> Mago
mejorOponente mago  = obtenerMaximo (\oponente -> abs (poder oponente - poder mago))

potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrima]}
weasley = Mago{nombre = "Ron", edad = 21, salud = 5, hechizos = [lagrima, sectum]}
malfoy = Mago{nombre = "Draco", edad = 22, salud = 10, hechizos = [lagrima, sectum]}
goyle = Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
granger = Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [lagrima, sectum, obliviateHechizo]}
snape = Mago{nombre = "Severus", edad = 50, salud = 200, hechizos = [sectum, obliviateHechizo, lagrima, confundusHechizo]}
dumbledore = Mago{nombre = "Albus", edad = 50, salud = 350, hechizos = [confundusHechizo, obliviateHechizo, lagrima, sectum]}
crabbe = Mago{nombre = "Vincent", edad = 9, salud = 6, hechizos = []}
rubeus = Mago{nombre = "Hagrid", edad = 40, salud = 200, hechizos = []}
bazarov = Mago{nombre = "Spartacus", edad = 45, salud = 120, hechizos = [confundusHechizo, lagrima, sectum]}
ibrahimova = Mago{nombre = "Stella", edad = 50, salud = 140, hechizos = [lagrima, sectum]}

lagrima = Hechizo{nombreHechizo = "lagrimaFenix", efecto = lagrimaFenix 30}
sectum = Hechizo{nombreHechizo = "sectumSempra", efecto = sectumSempra}
obliviateHechizo = Hechizo{nombreHechizo = "obliviate", efecto = obliviate 2}
confundusHechizo = Hechizo{nombreHechizo = "confundus", efecto = confundus}

noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago oponente = all (\hechizo -> salud mago == salud (efecto hechizo mago)) (hechizos oponente)


