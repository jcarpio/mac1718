{- ------------------------------------------- -}

-- DECLARACIÓN
noNegativo::(Num a, Ord a)=>a->Bool

{- PROPÓSITO
   Devuelve True si x es >= 0, False en otro caso
-}
-- DEFINICIÓN

noNegativo x = x >= 0

{-PRUEBAS
pru1 = positivo (–2.5) -- devuelve False
pru2 = positivo 0-- devuelve True
pru3 = positivo 5-- devuelve True
-}
{- ------------------------------------------- -}