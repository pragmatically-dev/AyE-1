{-
    Nombre:   Santiago Emanuel
    Apellido: NIEVA
    
-}

-- PROYECTO 1
--------------------------------------------------------------------------------------------------------------------
--Ejercicios:

{-
    1. Programa las siguientes funciones:
        a) esCero :: Int -> Bool, que verifica si un entero es igual a 0
        b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0
        c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula
        d) valorAbsoluto :: Int -> Int, que devuelve el valor absoluto de un entero ingresado
-}
-- esCero:
esCero:: Int -> Bool 
esCero x = x==0
{-
    Ejecucion:
    ghci> esCero 4  False
    ghci> esCero 9  False
    ghci> esCero 0  True
-}

-- esPositivo:
esPositivo:: Int -> Bool 
esPositivo x = x>0
--------------------------------------------------------------------------------------------------------------------
{-
    Ejecucion:
    ghci> esPositivo 6      True  
    ghci> esPositivo 0      False
    ghci> esPositivo (-4)   False
-}
--------------------------------------------------------------------------------------------------------------------
-- esVocal:

-- [vocales] cadena de caracteres que contiene a las vocales en minuscula
vocales::String
vocales = "aeiou"
-- [esVocal] utilizando la funcion [elem] provista por el lenguaje
esVocal::Char -> Bool
esVocal x = x `elem` vocales
--------------------------------------------------------------------------------------------------------------------
{-
Ejecucion: 
    ghci> esVocal 'a'   Output: True  
    ghci> esVocal 'e'   Output: True  
    ghci> esVocal 'i'   Output: True
    ghci> esVocal 'o'   Output: True
    ghci> esVocal 'u'   Output: True
    ghci> esVocal 'f'   Output: False
-}
--------------------------------------------------------------------------------------------------------------------

-- [esVocalAux] funcion auxiliar que comprueba recursivamente si [x] se encuentra en la lista de caracteres provista.
esVocalAux ::Char ->[Char]->Bool
esVocalAux x [] = False
esVocalAux x (v:vocales) | x /= v = esVocalAux x vocales 
                         | otherwise= True     

-- [esVocal'] Version utilizando composicion con [esVocalAux] con el fin de respetar la definicion pedida en el ejercicio 
esVocal'::Char -> Bool
esVocal' x = esVocalAux x vocales
--------------------------------------------------------------------------------------------------------------------
{-
Ejecucion:
    ghci> esVocal' 'a'  Output: True  
    ghci> esVocal' 'e'  Output: True  
    ghci> esVocal' 'i'  Output: True
    ghci> esVocal' 'o'  Output: True
    ghci> esVocal' 'u'  Output: True
    ghci> esVocal' 'd'  Output: False
-}
--------------------------------------------------------------------------------------------------------------------

-- valorAbsoluto: 

valorAbsoluto::Int -> Int
valorAbsoluto x | x>0 = x
                | x<0 = x*(-1)
                | otherwise = 0
{-
Ejecucion:
ghci> valorAbsoluto 12    Output: 12    
ghci> valorAbsoluto 0     Output: 0     
ghci> valorAbsoluto (-5)  Output: 5
-}
--------------------------------------------------------------------------------------------------------------------
{-
Ejercicio 2:
    a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True
    b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una lista de enteros
    c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de la lista de enteros
    d) factorial :: Int -> Int, que toma un numero n y calcula n!
    e) promedio :: [Int] -> Int, que toma una lista de enteros no vacia y calcula el promedio (truncado)
-}

-- paratodo
paratodo::[Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs
{-
Ejecucion:
ghci> paratodo [True,True,True]    Output: True
ghci> paratodo [True,True,False]   Output: False
ghci> paratodo [True,False,True]   Output: False
ghci> paratodo [True,True,True]    Output: True
ghci> paratodo [True]              Output: True
ghci> paratodo []                  Output: True
-}

-- sumatoria
sumatoria::[Int] -> Int
sumatoria [] = 0 --Elemento neutro de la suma
sumatoria (x:xs) = x + sumatoria xs

{-
Ejecucion:
ghci> sumatoria [1,5,-4]  Output: 2
ghci> sumatoria [2,4,1]   Output: 7
-}


-- productoria
productoria::[Int] -> Int
productoria [] = 1 --Elemento neutro de la multiplicacion
productoria (x:xs) = x * productoria xs

{-
Ejecucion:
ghci> productoria [1,2,3,4] Output: 24    
ghci> productoria [1,2,3]   Output: 6
-}

-- factorial
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = productoria [1..x]

{-
Ejecucion:
ghci> factorial 4   Output: 24
ghci> factorial 5   Output: 120
ghci> factorial 6   Output: 720
ghci> factorial 9   Output: 362880

-}

-- promedio:
promedio::[Int] -> Int
promedio [] = error "La lista no debe estar vacia."
promedio (x:xs) = sumatoria xs `div` length xs

{-
Ejecucion:
ghci> promedio [2,2,3,3]    Output: 2     
ghci> promedio [2,2,3,5]    Output: 3
-}
---------------------------------------------------------------------------------------------

{-
Ejecicio 3:
    Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se encuentra en una lista
-}

-- pertenece
pertenece :: Int -> [Int] -> Bool 
pertenece e [] = False
pertenece e (x:xs) | x /= e = pertenece e xs
                   | otherwise = True
{-
Ejecucion:
ghci> pertenece 4 [2,4,6]   Output: True  
ghci> pertenece 6 [2,4,6]   Output: True  
ghci> pertenece 7 [2,4,6]   Output: False
-}


{-
Ejercicio 4:
Programa las siguientes funciones que implementan los cuantificadores generales. Nota que
el segundo parametro de cada funcion, es otra funcion!
    a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.
    b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si alg ́un elemento de xs satisface el predicado t.
    c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una funci ́on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
    d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la
    aplicacion de t a los elementos de xs.
-}

--paraTodo'
paraTodo' :: [a] -> (a->Bool) -> Bool
paraTodo' [] _ = True
paraTodo' (x:xs) predicado | predicado x = paraTodo' xs predicado
                           | otherwise = False

{-
Ejecucion:
ghci> paraTodo' "hola" esVocal      Output: False 
ghci> paraTodo' [0,0,0,0] esCero    Output: True  
ghci> paraTodo' [0,0,1,0] esCero    Output: False
-}

-- existe'
existe' :: [a] -> (a->Bool) -> Bool
existe' [] _ = False
existe' (x:xs) predicado | not(predicado x)= existe' xs predicado
                         | otherwise = True 

-- Predicados de prueba:
mayorQue3:: Int ->Bool
mayorQue3 x = x>3

contieneR :: String -> Bool
contieneR x = 'r' `elem` x
----------------------------------------------------------------------------------------------
{-
Ejecucion:
ghci> existe' "" esVocal                            Output: False
ghci> existe' ["paloma","auto","motor"] contieneR   Output: True  
ghci> existe' ["paloma","auto","moto"] contieneR    Output: False
ghci> existe' [2,2,2,2,2,2,1] mayorQue3             Output: False
ghci> existe' [2,2,2,2,2,2,5] mayorQue3             Output: True
-}
-----------------------------------------------------------------------------------------------

-- sumatoria
sumatoria' :: [a] -> (a->Int) -> Int
sumatoria' [] _ = 0 -- rango vacio
sumatoria' (x:xs) predicado = predicado x + sumatoria' xs predicado

-- Predicados de prueba
sumar1::Int->Int
sumar1 x = x + 1

{-
Ejecucion:
ghci> sumatoria' [1,1,1,1] sumar1    Output: 8
ghci> sumatoria' [2,2,2] factorial   Output: 6     
ghci> sumatoria' [3,3,3] factorial   Output: 18
-}
----------------------------------------------------------------------------------------------

productoria' :: [a] -> (a->Int) -> Int
productoria' [] _ = 1 -- rango vacio
productoria' (x:xs) predicado = predicado x * productoria' xs predicado
{-
Ejecucion:
ghci> productoria' [2,3,4,5] factorial  34560 
ghci> productoria' [2,2,2] factorial    8     
ghci> productoria' [2,2,3] factorial    24
ghci> productoria' [3,3,3] factorial    216
-}
----------------------------------------------------------------------------------------------


{-
Ejercicio 5: 
    Defini nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo’ (sin recursion ni an ́alisis por casos!).

Recordamos la definicion de dicha funcion:
paratodo::[Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

-}
-- Predicado 
esTrue::Bool -> Bool
esTrue x = x && True

paraTodo''::[Bool] ->Bool
paraTodo'' xs = paraTodo' xs esTrue
{-
Ejecucion:
ghci> paraTodo'' [True,False,True]  Output: False 
ghci> paraTodo'' [True,True]        Output: True  
ghci> paraTodo'' []                 Output: True
-}
----------------------------------------------------------------------------------------------

{-
Ejercicio 6:

    Utilizando las funciones del ejercicio 4, program ́a las siguientes funciones por composicion, sin usar recursion ni analisis por casos.
        a) todosPares :: [Int] -> Bool verifica que todos los numeros de una lista sean pares.
        
        b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algun numero dentro del segundo parametro que sea multiplo del primer parametro.

        c) sumaCuadrados :: Int -> Int, dado un numero no negativo n, calcula la suma de los primeros n cuadrados,

        d) Programar la fucion existeDivisor::Int-> [Int] -> Bool, que dado en entero n y una lista ls , devuelve True si y solo si, existe algun elemento en ls que divida a n.

        e) Utilizando la funcion del apartado anterior, defini la funcion esPrimo:: Int -> Bool, que dado un entero n, devuelve True si y solo si n es primo.      

        f ) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursion?

        g) Programar la funcion multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los numeros primos de una lista.        

        h) Programar la funcion esFib :: Int -> Bool, que dado un entero n, devuelve True si y solo si n est ́a en la sucesi ́on de Fibonacci.

        i) Utilizando la funci ́on del apartado anterior, defini la funcion todosFib :: [Int] -> Bool que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen (o no) a la sucesion de Fibonacci.
-}

-- a) todos pares
esPar :: Int -> Bool
esPar x =  (x `mod` 2) == 0

todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs esPar
{-
Ejecucion:
ghci> todosPares [2,4,6,9]  Output: False
ghci> todosPares [2,4,6,8]  Output: True
-}
---------------------------------------------------------------------------------------------

-- b) hay multiplo
{-
Recordamos que:
Dados dos enteros x e y decimos que y es un divisor de x y escribimos y|x si:
    
    x = y * q, para algun q entero.

Tambien decimos que x es un MULTIPLO de y.

a = 0 (mod b) <=> m|a  =>  a es un multiplo de m
-}
esMultiplo::Int->Int->Bool
esMultiplo a b = a `mod` b == 0

hayMultiplo::Int->[Int] ->Bool
hayMultiplo x xs = existe' xs (  `esMultiplo` x)

{-
Ejecucion:
ghci> hayMultiplo 2 [3,8,16,18]   Output: True
ghci> hayMultiplo 2 [3,9,19]      Output: False 
ghci> hayMultiplo 3 [3,9,19]      Output: True
-}
---------------------------------------------------------------------------------------------

-- c) sumaCuadrados


generarLista::Int->[Int]
generarLista 0 = []
generarLista x = x:generarLista (x-1)

predicadoSumCuadrados::Int ->Int
predicadoSumCuadrados x = x^2

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' (reverse (generarLista (n-1))) predicadoSumCuadrados
 
{-
Ejecucion:
ghci> sumaCuadrados 4   Output: 14    
ghci> sumaCuadrados 8   Output: 140   
ghci> sumaCuadrados 5   Output: 30
-}
--------------------------------------------------------------------------------------------

-- d) existeDivisor

existeDivisor::Int->[Int]->Bool
existeDivisor n ls = existe' ls ( n `esMultiplo`) 
{-
Ejecucion: 
ghci> existeDivisor 18 [5,7,9] Output: True
ghci> existeDivisor 18 [5,7]   Output: False
-}
--------------------------------------------------------------------------------------------

-- e) esPrimo
esPrimo::Int -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo p = not(existeDivisor p [2..p-1])
{-
Ejecucion:
ghci> esPrimo 12    Output: False
ghci> esPrimo 13    Output: True
ghci> esPrimo 19    Output: True
ghci> esPrimo 23    Output: True
ghci> esPrimo 24    Output: False
-}

-- f)

factorial' :: Int -> Int
factorial' x = productoria [1..x]

