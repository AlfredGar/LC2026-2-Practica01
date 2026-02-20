module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle rad) = pi * (rad * rad)
area (Square l) = l * l
area (Rectangle b h) = b * h
area (Triangle l) = (sqrt 3 / 4) * (l * l)
area (Trapeze b1 b2 h) = ((b1 + b2) / 2) * h 

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter = undefined


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)


-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 p = distance (0.0, 0.0) p 

--Ejercicio 3
data Haskellium = Haskellium{
    nombre :: String, 
    apellido1 :: String, 
    apellido2 :: String,
    casita :: Point,
    forma :: Shape 
} deriving (Show)


--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son progenitor1 progenitor2 hijo = Haskellium {
    nombre = hijo,
    apellido1 = apellido1 progenitor1,
    apellido2 = apellido2 progenitor2,
    casita = casita progenitor1,
    forma = forma progenitor1
}

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined 

areaParedes :: Haskellium -> Float
areaParedes ciudadano = perimeter (casita ciudadano)

areaTecho :: Haskellium -> Float
areaTecho ciudadano = area (casita ciudadano)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo = undefined

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr = undefined

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Undefinedd

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined
