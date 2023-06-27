import Data.Char

f :: Bool -> Bool
f x = not x

f2 :: Bool -> Float
f2 x = pi

funcion3 :: Integer -> Integer -> Bool -> Bool
funcion3 x y b = b || (x > y)

doble :: Float -> Float
doble x = x + x

cuadruple :: Float -> Float
cuadruple x = doble (doble x)

dist :: Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

doble2 :: Num a => a -> a
doble2 x = x + x

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir p = (snd p,fst p)

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia p1 p2 = sqrt ((fst p2 - fst p1)^2 + (snd p2 + snd p1)^2)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = ((-b + sqrt (b^2 - 4*a*c)) / (2*a), (-b - sqrt (b^2 - 4*a*c))/ (2*a))

listar :: a -> a -> a -> [a]
listar x y z = [x,y,z]

rangoDePaso :: Integer -> Integer -> Integer -> [Integer]
rangoDePaso n1 n2 n3 = [n1, n1 + n3..n2]

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente p1 p2 = (snd p2 - snd p1)/(fst p2 - fst p1)

iniciales :: String -> String -> String
iniciales a b = [head a, '.', head b, '.']