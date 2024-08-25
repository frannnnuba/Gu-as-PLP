--Ej 2
valorAbsoluto :: Float -> Float
valorAbsoluto n | n > 0 = n
                | otherwise = n * (-1)

añoBisiesto :: Int -> Bool
añoBisiesto n | (mod n 4 == 0 && mod n 100 /= 0) || (mod n 400 == 0) = True
              | otherwise = False
              -- con el caso cero da true no se xq porque no deberia

factorial :: Int -> Int 
factorial 1 = 1
factorial n = n * factorial(n-1)


esPrimo :: Int -> Int -> Bool
esPrimo _ 1 = True
esPrimo n y | mod n 2 == 0 && n /= 2 = False
            | mod n y == 0 = False 
            | otherwise = esPrimo n (y-1)

--divisoresPrimos :: Int -> Int 
--divisoresPrimos 1 = 0
--divisoresPrimos 2 = 1
--divisoresPrimos n | esPrimo n (n-1) = 0
 --                 | mod n 2 == 0 && n /= 2 = 0
  --                | mod n x == 0 && esPrimo x (x-1) = DivisoresPrimos
   --               | otherwise 
--Ej3
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero x = case x of
          (Left n) -> n
          (Right b) -> if b then 1 else 0 

-- ej 4
limpiar :: String -> String -> String
limpiar [] n = n
limpiar (x:xs) y | elem x y = limpiar xs (eliminar x y)
                 | otherwise = limpiar xs y

eliminar :: Char -> String -> String 
eliminar _ [] = [] 
eliminar x (y:ys)| x == y = eliminar x ys 
                 | otherwise = y : eliminar x ys 

promedio :: [Float] -> Float -> Float
promedio [] y = y
promedio (x:xs) y = promedio xs ((x + y) / 2)

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio (x:xs) = (x - promedio xs x) : difPromedio xs

--Ej 5
data AB a = Nil | Bin (AB a) a (AB a)
vacio :: AB a -> Bool 
vacio Nil = True
vacio x = False


negacion :: AB Bool -> AB Bool
negacion Nil = Nil
--negacion (n:ns) | n == True = False : negacion ns
--                | otherwise = True : negacion ns



-------- guia 1
max2 :: (Int,Int) -> Int 
max2 (x,y) | x >= y = x
           | otherwise = y 