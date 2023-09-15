{-
Alumno: Santiago Emanuel Nieva
Proyecto 2
-}


-- Ejercicios:

{-
1. Tipos enumerados. Cuando los distintos valores que debemos distinguir en un tipo son
finitos, podemos enumerar cada uno de los valores del tipo. Por ejemplo, podríamos representar
las carreras que se dictan en nuestra facultad definiendo el siguiente tipo:
data Carrera = Matemática | Física | Computación | Astronomía
Cada uno de estos valores es un constructor, ya que al utilizarlos en una expresión, generan
un valor del tipo Carrera.
    
    a) Implementa el tipo Carrera como está definido arriba.
    
    b) Define la siguiente función, usando pattern matching: titulo :: Carrera -> String
    que devuelve el nombre completo de la carrera en forma de string. Por ejemplo, para el
    constructor Matemática, debe devolver "Licenciatura en Matemática".

    c) Para escribir música se utiliza la denominada notación musical, la cual consta de
    notas (do, re, mi, ...). Además, estas notas pueden presentar algún modificador # (sostenido) o b (bemol), por ejemplo do#, si b, etc. Por ahora nos vamos a olvidar de
    estos modificadores (llamados alteraciones) y vamos a representar las notas básicas.
    Definir el tipo NotaBasica con constructores Do, Re, Mi, Fa, Sol, La y Si.

    d) El sistema de notación musical anglosajón, también conocido como notación o cifrado
    americano, relaciona las notas básicas con letras de la A a la G. Este sistema se usa por
    ejemplo para las tablaturas de guitarra. Programa usando pattern matching la función:
    cifradoAmericano :: NotaBasica -> Char
    que relaciona las notas Do, Re, Mi, Fa, Sol, La y Si con los caracteres 'C', 'D', 'E',
    'F', 'G', 'A' y 'B' respectivamente.

-}

-- a)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Evaluate" #-}


data Carrera = Matematica|Fisica|Computacion|Astronomia 
prefix :: String
prefix= "Licenciatura en "

-- b)
titulo :: Carrera -> String
titulo Matematica   = prefix ++ "Matematica"
titulo Fisica       = prefix ++ "Fisica"
titulo Computacion  = prefix ++ "Computacion"
titulo Astronomia   = prefix ++ "Astronomia"
{-
ghci> titulo Matematica
"Licenciatura en Matematica"
ghci> titulo Computacion 
"Licenciatura en Computacion"
-}



-- c)
data NotaBasica = Do |Re| Mi| Fa| Sol| La| Si

--d)
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'
{-
ghci> cifradoAmericano Do
'C'   
ghci> cifradoAmericano La
'A'
-}


{-
 Clases de tipos
-}

data NotaBasica' = DO |RE| MI| FA| SOL| LA| SI deriving (Eq,Ord,Bounded,Enum,Show)
{-
ghci> FA `min` SOL
FA    
ghci> FA <= SOL   
True
-}

---------------------------------------------------------------------------------------------

{- POLIMORFISMO AD HOC:
Este tipo de definiciones se llaman polimorfismo ad hoc, ya que no es una definición completamente genérica.

a) Definir usando polimorfismo ad hoc la función minimoElemento que calcula (de manera recursiva) cuál es el menor valor de una lista de tipo [a]. Asegurarse que sólo esté definida para listas no vacías.

b) Definir la función minimoElemento’ de manera tal que el caso base de la recursión sea el de la lista vacía. Para ello revisar la clase Bounded.

Ayuda: Para probar esta función dentro de ghci con listas vacías, indicar el tipo concreto con tipos de la clase Bounded, por ejemplo: ([1,5,10]::[Int]), ([]::[Bool]), etc.

c) Usar la función minimoElemento para determinar la nota más grave de la melodía: [Fa, La, Sol, Re, Fa]

En las definiciones de los ejercicios siguientes, deben agregar deriving sólo cuando sea estrictamente necesario.

-}

-- a)

minimo :: (Eq a,Ord a) => a-> a->a
minimo a b |a>b  = b 
           |a<b  = a
           |a==b = a

minimoElemento :: (Eq a,Ord a) => [a] -> a
minimoElemento []       = error "La lista no debe ser vacia"
minimoElemento [x]      = x
minimoElemento (x:xs)   = minimo x (minimoElemento xs)
{-
ghci> minimoElemento [1,2,3,4]
1     
ghci> minimoElemento [SOL,LA,SI,DO]
DO
-}

--b)
minimoElemento' :: (Eq a,Ord a,Bounded a) => [a] -> a
minimoElemento' []       = maxBound
minimoElemento' (x:xs)   = minimo x (minimoElemento' xs)

{-
ghci> minimoElemento [1,2,3,4]
1     
ghci> minimoElemento [SOL,LA,SI,DO]
DO
-}

--c)
{-
ghci> minimoElemento' [FA,LA,SOL,RE,FA]
RE
-}
--------------------------------------------------------------------------------------------

{-
    Ejercicio 4
-}

-- Sinónimos de tipo
type Altura = Int
type NumCamiseta = Int

-- Tipos algebraicos sin parámetros
data Zona           = Arco | Defensa | Mediocampo | Delantera   deriving (Show,Eq)
data TipoReves      = DosManos | UnaMano                        deriving (Show,Eq)
data Modalidad      = Carretera | Pista | Monte | BMX           deriving (Show,Eq) 
data PiernaHabil    = Izquierda | Derecha                       deriving (Show,Eq)

-- Sinónimo
type ManoHabil = PiernaHabil

-- Tipo Deportista con constructores paramétricos
data Deportista = Ajedrecista
                | Ciclista Modalidad
                | Velocista Altura
                | Tenista TipoReves ManoHabil Altura
                | Futbolista Zona NumCamiseta PiernaHabil Altura 
                deriving (Show,Eq)

-- b)
{-
    ghci> :t Ciclista 
    Ciclista :: Modalidad -> Deportista
-}

--c)
contar_velocistas :: [Deportista] -> Int
contar_velocistas []                 = 0
contar_velocistas (Velocista _ : xs) = 1 + contar_velocistas xs
contar_velocistas (_:xs)             = contar_velocistas xs

deportistas :: [Deportista]
deportistas = [Velocista 180, Ciclista Carretera, Ajedrecista, Velocista 175, Tenista DosManos Izquierda 188]
{-
ghci> contar_velocistas deportistas 
2
-}

-- e)
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas (Futbolista Arco _ _ _  : xs)        Arco        = 1 + contar_futbolistas xs Arco
contar_futbolistas (Futbolista Defensa _ _ _  : xs)     Defensa     = 1 + contar_futbolistas xs Defensa
contar_futbolistas (Futbolista Mediocampo _ _ _  : xs)  Mediocampo  = 1 + contar_futbolistas xs Mediocampo
contar_futbolistas (Futbolista Delantera _ _ _  : xs)   Delantera   = 1 + contar_futbolistas xs Delantera
contar_futbolistas (_ : xs) zona                           = contar_futbolistas xs zona

{-
ghci> let p1 = Futbolista Delantera  10 Izquierda 180
ghci> let p2 = Futbolista Arco   1 Izquierda 190     
ghci> let p3 = Futbolista Delantera    9 Izquierda 160
ghci> lista_de_futbolistas = p1:p2:p3:[]  
ghci> lista_de_futbolistas 

[Futbolista Delantera 10 Izquierda 180,Futbolista Arco 1 Izquierda 190,Futbolista Delantera 9 Izquierda 160]

ghci> contar_futbolistas lista_de_futbolistas  Delantera  
2 
-}

--e) 



esFutbolistaYcoincideZona  :: Deportista ->Zona -> Bool
esFutbolistaYcoincideZona (Futbolista z _ _ _ ) zona = True && (z == zona)
esFutbolistaYcoincideZona _ _ = False   



contar_futbolistas' ::[Deportista] -> Zona -> Int
contar_futbolistas' xs z = length(  filter (\x -> esFutbolistaYcoincideZona x z) xs)
{-
ghci> let p1 = Futbolista Delantera  10 Izquierda 180
ghci> let p2 = Futbolista Arco   1 Izquierda 190     
ghci> let p3 = Futbolista Delantera    9 Izquierda 160
ghci> lista_de_futbolistas = p1:p2:p3:[]  
ghci> lista_de_futbolistas 

[Futbolista Delantera 10 Izquierda 180,Futbolista Arco 1 Izquierda 190,Futbolista Delantera 9 Izquierda 160]

ghci> contar_futbolistas' lista_de_futbolistas  Delantera  
2 
-}