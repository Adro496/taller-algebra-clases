doble x = 2 * x

suma x y = x + y

normaVectorial v1 v2 = sqrt(v1^2 + v2^2)

funcionConstante8 x = 8

respuestaATodo = 42

signo n | n == 0 = 0
        | n > 0 = 1
        | n < 0 = -1

modulo n = n * (signo n)

modulo2 n   | n >= 0 = n
            | otherwise = -n

maximo x y  | x >= y = x
               | otherwise = y

maximo3 x y z = maximo (maximo x y) z

esPositivo x = x > 0

yLogico x y | x == False = False
            | y == False = False
            | otherwise = True

yLogico2 x y    | x == False = False
                | otherwise = y

yLogico3 False y = False
yLogico3 True y = y

yLogicoCompacto x y = x&&y

funcion3Variables n1 n2 n3  | n2 < 10 = n1
                            | n2 >= 10 = n1 + n3

nand x y    | x == False = True
            | y == False = True
            | otherwise = False

nor True True = False
nor True False = False
nor False True = False
nor False False = True

algunaRaiz a b c = (-b + sqrt(b^2 - 4*a*c)) / (2*a)

esPitagorica a b c = (a^2 + b^2) == c^2