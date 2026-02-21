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
perimeter (Circle circulito) = 2 * pi * circulito
perimeter (Square ladito) = 4 * ladito 
perimeter (Rectangle base altura) = 2 * (base + altura)
perimeter (Triangle ladote) = 3 * ladote
--perimeter (Trapeze base al1 al2) = --no le supe


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
houseCost ciudadano = areaParedes ciudadano + areaTecho ciudadano

areaParedes :: Haskellium -> Float
areaParedes ciudadano = perimeter (forma ciudadano) * 2.5

areaTecho :: Haskellium -> Float
areaTecho ciudadano = area (forma ciudadano)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork ciudadano = 
        let dist = from0 (casita ciudadano)
        in if dist < 300.0
           then dist / 30.0
           else dist / 70.0 
                

--LISTAS Y FUNCIONES
--Ejercicio 1

--Función auxiliar para sacar la reversa de una cadena
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


palindromo :: String -> Bool
palindromo [] = True
palindromo [_] = True
palindromo xs = xs == reversa xs 

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

--Ejercicio 3

--Función auxiliar para combinar cadenas.

combinar :: a -> [[a]] -> [[a]]
combinar x [] = []
combinar x (y:ys) = (x:y) : y : (combinar x ys)


conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = combinar x (conjuntoPotencia xs)

--ARBOLES

--Implementacion

data OneTwoTree a = Vacio | Nodo a (OneTwoTree a) | Rama a (OneTwoTree a) (OneTwoTree a) deriving (Show, Eq)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Vacio = 0
suma (Nodo x hijo) = x + suma hijo
suma (Rama x izq der) = x + suma izq + suma der
