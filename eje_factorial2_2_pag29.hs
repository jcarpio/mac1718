
factorial n
  |n== 0  = 1
  |n > 0 = facto where facto = n * factorial (n-1)
  |otherwise = error "valor negativo"