
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
