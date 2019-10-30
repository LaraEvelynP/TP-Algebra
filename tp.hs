
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