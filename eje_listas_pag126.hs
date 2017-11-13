

-- 1. Implementar la función divisores que recibe un argumento entero y 
-- que devuelva la lista de sus divisores.

divisores x = [n | n <- [1..x], x `mod` n == 0]

--  2. Utilizando la función anterior, programar la función primo que devuelva 
-- verdadero en caso  de  que  su  único  argumento  entero  sea  un  número  primo.  
-- No  consideraremos  al número 1 como primo.

primo x = length (divisores x) == 2

-- 3. Crear una expresión con la que se obtengan los primos entre 1 y 100. Utilizar la 
-- notación extendida de listas para este ejercicio.

primos_x n = [x | x <- [1..n], primo x]

-- 4. Averiguar cómo funcionan las funciones map y filter e implementarlas utilizando la 
-- notación extendida de listas. Llamar a las nuevas funciones mapea y filtra.

mapea f l = [f x | x <- l]

filtra f l = [x | x <- l, f x]

-- 5. Programar una función evaluaciones con la siguiente cabecera 
-- evaluaciones::[a]->[(a->b)]->[[b]]
-- La lista de listas resultante contiene listas con los resultados de aplicar
-- a cada uno de los valores de la primera lista las funciones de la segunda lista. Por ejemplo:
-- Main> evaluaciones [1,2,3] [doble, triple]
-- [[2,3],[4,6],[6,9]]

doble x = x + x
triple x = x + x + x
 
evaluaciones v lf = [[f x | f <- lf]| x <- v]


-- 6. Utilizar la función anterior  para evaluar si los siguientes valores 
-- [0,(3.14/2),((-3.14)/2), 3.14,(-3.14)]  cumplen  que  el  seno  es  mayor  
-- que  0,  el  coseno  es  0  y  la  tangente  es  0.  
-- Componer la lista de funciones utilizando el operador de composición “.”. El resultado  
-- para este ejemplo será:
-- [[False,False,True],[True,False,False],[False,False,False],[True,False,False
-- ],[False,False,False]]

--  evaluaciones [0,(3.14/2),((-3.14)/2), 3.14,(-3.14)] [(>0).sin, (==0).cos, (==0).tan]

-- 7. Implementar  una  función  que  devuelva  la  descomposición  en  factores  primos
--  de un número entero. La función devolverá una lista de tuplas tal que la primera 
-- componente será el factor primo y la segunda será el número de veces que dicho factor 
-- primo divide al argumento original.

-- Main> descomposicion 60
-- [(2,2),(3,1),(5,1)]


num_div x y 
  | x `mod` y == 0 && x > y  = 1 + num_div (div x y) y
  | otherwise = 0

descomposicion x = [(n, num_div x n) | n <- primos_x x, x `mod` n == 0]


-- 8. Averiguar qué devuelven las funciones takeWhile y dropWhile
-- e implementar su propia versión. Llamar a las nuevas funciones 
-- tomarMientras y eliminarMientras

tomarMientras f l = [x| x <- l, f x ] 
-- eliminarMientras f l = [x| x <- l, f x == ]

constante1 x = 1


integral f a b t = sum [(f x) * t | x <- [a, a+t .. b-t]]