
--Ejercicio 1
data Forma = Piedra | Papel | Tijera

leGana ::Forma -> Forma -> Bool
leGana Piedra Tijera = True
leGana Piedra Piedra = False
leGana Piedra Papel  = False

leGana Papel Tijera  = False
leGana Papel Piedra  = True
leGana Papel Papel   = False

leGana Tijera Tijera = False
leGana Tijera Papel  = True
leGana Tijera Piedra = False

{-
ghci> leGana Piedra Papel 
False
ghci> leGana Papel Tijera 
False
ghci> leGana Papel Piedra 
True
-}


-- b)

type Nombre = String 
data Jugador = Mano Nombre Forma

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano j1 Papel ) (Mano j2 Papel)  = Nothing
ganador (Mano j1 Piedra) (Mano j2 Piedra) = Nothing
ganador (Mano j1 Tijera) (Mano j2 Tijera) = Nothing
ganador (Mano j1 f1) (Mano j2 f2) 
                                |   (leGana f1 f2)      = Just j1
                                |not(leGana f1 f2)      = Just j2  

p1 = (Mano "Santiago" Papel)
p2 = (Mano "X"     Piedra)
{-
ghci> ganador p1 p2
Just "Santiago"
-}