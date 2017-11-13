

-- 1. Implementar la función divisores que recibe un argumento entero y 
-- que devuelva la lista de sus divisores.

divisores x = [n | n <- [1..x], x `mod` n == 0]

--  2. Utilizando la función anterior, programar la función primo que devuelva 
-- verdadero en caso  de  que  su  único  argumento  entero  sea  un  número  primo.  
-- No  consideraremos  al número 1 como primo.

primo x = length (divisores x) == 2