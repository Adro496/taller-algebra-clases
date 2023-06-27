mcd :: Integer -> Integer -> Integer
mcd a b | b == 0 = a
        | div a b == 0 = mcd b a
        | otherwise = mcd b (mod a b)

first :: (a, b, c) -> a
first (x, y, z) = x

longitud :: [a] -> Integer
longitud [] = 0
longitud (x:[]) = 1
longitud (x:y:[]) = 2
longitud (x:y:z:[]) = 3
longitud (_:_:_:xs) = 3 + longitud xs

iniciales :: [Char] -> [Char] -> [Char]
iniciales nombre apellido = [n,'.', a]
                            where (n:_) = nombre
                                  (a:_) = apellido

type Racional = (Integer, Integer)
producto :: Racional -> Racional -> Racional
producto (a, b) (c, d) = (a*c, b*d)

igual :: Racional -> Racional -> Bool
igual (a, b) (c, d) = a * d == b * c

mayor :: Racional -> Racional -> Bool
mayor (a, b) (c, d) = a * d >= b * c

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show
esFinde :: Dia -> Bool
esFinde Sabado = True
esFinde Domingo = True
esFinde otherwise = False

diaHabil :: Dia -> Bool
diaHabil a = esFinde a == False

soloAlgebra :: [Dia] -> [Dia]
soloAlgebra a   | length a == 0 = []
                | auxSA (head a) == True = head a : soloAlgebra (tail a)
                | otherwise = soloAlgebra (tail a)

auxSA :: Dia -> Bool
auxSA Martes = True
auxSA Viernes = True
auxSA otherwise = False

tuplas :: [a] -> [b] -> [(a, b)]
tuplas xl yl    | length xl == 0 || length yl == 0 = []
                | otherwise = (head xl, head yl) : tuplas (tail xl) (tail yl)

potencia :: Racional -> Integer -> Racional
potencia (x, y) n = (x ^ n, y ^ n)

type Conjunto = [Integer]
union :: Conjunto -> Conjunto -> Conjunto
union xl yl | xl == [] = yl
            | yl == [] = xl
            | pertenece (head xl) yl == False = head xl : union (tail xl) yl
            | otherwise = union (tail xl) yl

pertenece :: Integer -> Conjunto -> Bool
pertenece a xl  | xl == [] = False
                | a == head xl = True
                | otherwise = pertenece a (tail xl)

interseccion :: Conjunto -> Conjunto -> Conjunto
interseccion xl yl  | xl == [] || yl == [] = []
                    | pertenece (head xl) yl == True = head xl : interseccion (tail xl) yl
                    | otherwise = interseccion (tail xl) yl

incluido :: Conjunto -> Conjunto -> Bool    --podría agregar length xl > length yl = False
incluido xl yl  | xl == [] = True
                | pertenece (head xl) yl == True = incluido (tail xl) yl
                | otherwise = False

igualdad :: Conjunto -> Conjunto -> Bool
igualdad xl yl = incluido xl yl && incluido yl xl

separar :: Integer -> Conjunto -> (Conjunto, Conjunto)
separar x xl    | xl == [] = ([], [])
                | head xl <= x = (head xl : fst txl, [] ++ snd txl)
                | otherwise = ([] ++ fst txl, head xl : snd txl)
                where txl = separar x (tail xl)