{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Test.Hspec
import Practica01
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do
        
    describe "Tests de area" $ do
        it "Circle con r=8" $ do
            truncateDecimals (area (Circle 8)) `shouldBe`201.06 
        
        it "Circle con r=3.5" $ do
            truncateDecimals (area (Circle 3.5)) `shouldBe` 38.48
        
        it "Square con l=5.2" $ do
            truncateDecimals (area (Square 5.2)) `shouldBe` 27.03
        
        it "Square con l=3" $ do
            truncateDecimals (area (Square 3)) `shouldBe` 9
        
        it "Rectangle con b=5, h=2" $ do
            truncateDecimals (area (Rectangle 5 2)) `shouldBe` 10
        
        it "Rectangle con b=8.6, h=9.4" $ do
            truncateDecimals (area (Rectangle 8.6 9.4)) `shouldBe` 80.84
        
        it "Triangle con l=5" $ do
            truncateDecimals (area (Triangle 5)) `shouldBe` 10.82
        
        it "Triangle con 3.3" $ do
            truncateDecimals (area (Triangle 3.3)) `shouldBe` 4.71
        
        it "Trapeze con b1=2, b2=5, h=3" $ do
            truncateDecimals (area (Trapeze 2 5 3)) `shouldBe` 10.5
        
        it "Trapeze con b1=2.5, b2=5.5, h=7.2" $ do
            truncateDecimals (area (Trapeze 2.5 5.5 7.2)) `shouldBe` 28.8

    describe "Tests de perimeter" $ do
        it "Circle con r=8" $ do
            truncateDecimals (perimeter (Circle 8)) `shouldBe`50.26
        
        it "Circle con r=3.5" $ do
            truncateDecimals (perimeter (Circle 3.5)) `shouldBe` 21.99
        
        it "Square con l=5.2" $ do
            truncateDecimals (perimeter (Square 5.2)) `shouldBe` 20.8
        
        it "Square con l=3" $ do
            truncateDecimals (perimeter (Square 3)) `shouldBe` 12
        
        it "Rectangle con b=5, h=2" $ do
            truncateDecimals (perimeter (Rectangle 5 2)) `shouldBe` 14
        
        it "Rectangle con b=8.6, h=9.4" $ do
            truncateDecimals (perimeter (Rectangle 8.6 9.4)) `shouldBe` 36
        
        it "Triangle con l=5" $ do
            truncateDecimals (perimeter (Triangle 5)) `shouldBe` 15
        
        it "Triangle con 3.3" $ do
            truncateDecimals (perimeter (Triangle 3.3)) `shouldBe` 9.89
        
        it "Trapeze con b1=2, b2=5, h=3" $ do
            truncateDecimals (perimeter (Trapeze 2 5 3)) `shouldBe` 13.70
        
        it "Trapeze con b1=2.5, b2=5.5, h=7.2" $ do
            truncateDecimals (perimeter (Trapeze 2.5 5.5 7.2)) `shouldBe` 22.70

    describe "Tests de distance" $ do
        it "Distancia entre los puntos (35,0) (70,0)" $ do 
            truncateDecimals (distance (35,0) (70,0)) `shouldBe` 35
        
        it "Distancia entre los puntos (10,25) (5.25,-3.4)" $ do 
            truncateDecimals (distance (10,25) (5.25,-3.4)) `shouldBe` 28.79

    describe "Tests de from0" $ do
        it "Distancia del origen al punto (10,25)" $ do
            truncateDecimals (from0 (10,25))  `shouldBe` 26.92

        it "Distancia del origen al punto (60,0)" $ do
            truncateDecimals (from0 (60,0))  `shouldBe` 60

    describe "Tests de Haskellium" $ do
        let gael =  Haskellium {name = "Gael", lastName1 = "Garcia", lastName2 = "Aguilera", location = (45,0), houseShape = Rectangle 20 8}
        let edu = Haskellium {name = "Eduardo", lastName1 = "Vargas", lastName2 = "Perez", location = (2,90), houseShape = Square 10}
        let fer = Haskellium {name = "Fernanda", lastName1 = "Solis", lastName2 = "Espinoza", location = (2,90), houseShape = Square 10}
        
        it "Hijo de edu y fer" $ do 
            let david = son edu fer "David" 
            name david `shouldBe` "David"
            lastName1 david `shouldBe` "Vargas"
            lastName2 david `shouldBe` "Solis"
            location david `shouldBe` (2,90)
            houseShape david `shouldBe` Square 10
        
        it "Costo casa de edu" $ do 
            truncateDecimals (houseCost edu) `shouldBe` 200

        it "Costo casa de gael" $ do 
            truncateDecimals (houseCost gael) `shouldBe`300

        it "Tiempo al trabajo de edu" $ do 
            truncateDecimals (timeToWork edu) `shouldBe` 3

        it "Tiempo al trabajo de gael" $ do 
            truncateDecimals (timeToWork gael) `shouldBe`1.5

    describe "Tests de palindromo" $ do 
        it "anitalavalatina es un palindromo" $ do
            palindromo "anitalavalatina" `shouldBe` True

        it "aibofobia es un palindromo" $ do
            palindromo "aibofobia" `shouldBe` True

        it "palindromo no es un palindromo" $ do
            palindromo "palindromo" `shouldBe` False

    describe "Tests de myFoldr" $ do 
        it "Verificar que algun elemento sea igual a 3" $ do
            myFoldr (\x acc -> (x == 3) || acc) False [1,2,3,4] `shouldBe` True

        it "Cadenas" $ do
            myFoldr (++) " mundo" ["H","o","l","a"] `shouldBe` "Hola mundo"

        it "Maximo" $ do
            myFoldr max 18 [3,6,12,4,55,11] `shouldBe` 55
        
    describe "Test conjuntoPotencia" $ do
        it "Lista enteros" $ do
            conjuntoPotencia [1,2] `shouldMatchList` [[1,2], [1], [2], []]
        
        it "Lista cadenas" $ do
            conjuntoPotencia ["x", "y", "z"] `shouldMatchList` [["x","y","z"], ["x","y"], ["x","z"], ["x"], ["y","z"], ["y"], ["z"], []]

        it "Cadena" $ do
            conjuntoPotencia "abc" `shouldMatchList` ["abc", "ab", "ac", "a", "bc", "b", "c", ""]

    describe "Tests de OneTwoTree" $ do
        let ottEmpt = Void
        let ottBranchInt = Branch 1 (Branch 2 Void Void) (Branch 3 Void Void)
        let ottCombined = Node 2 (Branch 5 (Node 4 Void) (Branch 8 Void (Node 1 (Node 0 Void))))

        it "suma" $ do
            suma ottEmpt `shouldBe` 0
            suma ottBranchInt `shouldBe` 6
            suma ottCombined `shouldBe` 20


truncateDecimals :: Float -> Float
truncateDecimals n = fromIntegral (truncate (n * 100)) / 100