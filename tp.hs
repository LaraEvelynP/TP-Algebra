
---------------EJERCICIO UNO----------------
type Circulo = [Integer]


sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales [] [] = True
sonCirculosIguales circulo1 circulo2
   | length circulo1 /= length circulo2 || distintos circulo1 circulo2  = False
   | x == y = comparar circulo1 circulo2
   | otherwise = sonCirculosIguales circulo1 (tail circulo2 ++ [head circulo2])
   where (x:xs) = circulo1
         (y:ys) = circulo2

distintos :: Circulo -> Circulo -> Bool
distintos _ [] = True 
distintos (x:xs) (y:ys) 
   | x /= y = distintos (x:xs) ys
   | otherwise = False


comparar :: Circulo -> Circulo -> Bool
comparar [] [] = True
comparar (x:xs) (y:ys)
   | x == y = comparar xs ys
   | otherwise = False

-----------------------------------------------------


circuloPrimo :: Circulo -> Bool
circuloPrimo [] = True
circuloPrimo (x:y:xs) = esPrimo (x + y) && circuloPrimo xs

esPrimo :: Integer -> Bool
esPrimo n = sumaLista (divisoresHasta n n) == n+1

sumaLista :: [Integer] -> Integer
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

divisoresHasta :: Integer -> Integer -> [Integer]
divisoresHasta n m | m == 0 = []
                   | mod n m == 0 = m : divisoresHasta n (m-1)
                   | otherwise = divisoresHasta n (m-1)
