-- Definición de las funciones para tomar partes del número
tomarTresUlt :: Double -> Int
tomarTresUlt x = round x - ((floor (x / 1000)) * 1000)

tomarTresPrim :: Double -> Int
tomarTresPrim x = floor (x / 100000)

tomarDos :: Int -> Int
tomarDos x = mod (div x 1000) 100

-- Definición de las funciones para buscar divisores
buscarDivisores :: Int -> [Int]
buscarDivisores x = divisoresRec x x []

divisoresRec :: Int -> Int -> [Int] -> [Int]
divisoresRec x y xs
    | y == 1 = 1 : xs
    | mod x y == 0 = divisoresRec x (y - 1) (y : xs)
    | otherwise = divisoresRec x (y - 1) xs

-- Identificación basada en divisores
identificar2 :: [Int] -> String
identificar2 xs
    | sum1 == last xs = "Engineering"
    | sum1 < last xs = "Humanities"
    | sum1 > last xs = "Administrative"
    where sum1 = sum (init xs)

-- Función para convertir Double a Int
doubleToInt :: Double -> Int
doubleToInt x = floor x

enteroADouble :: Int -> Double
enteroADouble x = fromIntegral x


--Funcion para completar Programa Académico
completarProgramaAcademico :: Double -> Int
completarProgramaAcademico y = (tresPrim * 100000) + (mitad * 1000) + tresUlt
    where
        tresPrim = floor (y / 1000)
        tresUlt = round y - (floor (y / 1000) * 1000)
        mitad = mod (floor (y / 1000)) 10
        
verificarProgramaAcademico :: Double -> Int
verificarProgramaAcademico y
    |y < 10000000 = completarProgramaAcademico y
    |otherwise = doubleToInt y


--Funciones para completar Numero de Admisiones
completarNumeroAdmisiones1 :: Int -> Int
completarNumeroAdmisiones1 y = (tresPrim*100000) + (mitad*1000) + ultNum
    where
        tresPrim = div y 1000
        ultNum = mod y 10
        mitad = div (mod y 1000) 10
   
   
completarNumeroAdmisiones2 :: Int -> Int
completarNumeroAdmisiones2 y = (tresPrim*100000) + (mitad*1000) + dosUlt
    where
        tresPrim = div y 10000
        dosUlt = mod y 100
        mitad = mod (div y 100) 100
        
completarNumeroAdmisiones y
    |y < 1000000 = completarNumeroAdmisiones1 y
    |y < 10000000 = completarNumeroAdmisiones2 y
    |otherwise = y

-- Función identificar1
identificar1 :: Int -> String
identificar1 x = identificar2 (buscarDivisores (tomarDos x))

-- Función para verificar si un número es par
verificarPar :: Int -> String
verificarPar x
    | even x = "even"
    | otherwise = "odd"

convertirAno :: Int -> Bool -> Int
convertirAno x bool
    | bool = x `mod` 10
    | otherwise = div x 10
    
    
-- Función principal
main :: IO ()
main = do

    yStr <- getLine
    let y = read yStr :: Int
    
    let z = enteroADouble (completarNumeroAdmisiones y)
    
    let x = enteroADouble (verificarProgramaAcademico z) 
    
    let xInt = doubleToInt x  -- Convertir el Double a Int para usar en las funciones que necesitan Int
    putStr $ show (20)
    putStr $ show (convertirAno (tomarTresPrim x) False)
    putStr $ "-"
    putStr $ show (convertirAno (tomarTresPrim x) True)
    putStr $ " "
    putStr $ identificar1 xInt
    putStr $ " num"
    putStr $ show (tomarTresUlt x)
    putStr $ " "
    putStr $ verificarPar xInt
    
    
    
