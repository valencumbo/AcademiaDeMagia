module Academia where

import PdePreludat
import Data.Int

data Mago = Mago{
    nombre :: String,
    edad :: Int,
    salud :: Int,
    hechizos :: [Hechizo]
} deriving (Show)

data Hechizo = Hechizo{
    nombreHechizo :: String,
    efecto :: EfectoHechizo
} deriving (Show)

type EfectoHechizo = Mago -> Mago