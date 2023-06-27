fib :: Integer -> Integer
fib n   | n == 0 = 1
        | n == 1 = 1
        | n == 40 = 165580141 --no calcular 41
        | n > 1 = fib (n - 1) + fib (n - 2)

par :: Integer -> Bool
par n   | n == 0 = True
        | n == 1 = False
        | n > 1 = par (n - 2)
        | n < 0 = par (n + 2)

sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = sumaImpares (n - 1) + (2*n - 1)

multiploTres :: Integer -> Bool
multiploTres n  | n == (-1) = False
                | n == 0 = True
                | n == 1 = False
                | n > 1 = multiploTres (n - 3)
                | n < 0 = multiploTres (n + 3)

doblefact n | n == 2 = 2
            | n > 2 = n * doblefact (n - 2)

combinatorio :: Integer -> Integer -> Integer
combinatorio n m    | m == n = 1
                    | m == 0 = 1
                    | m == 1 = n
                    | otherwise = combinatorio (n - 1) m + combinatorio (n - 1) (m - 1)

sirvoParaNaturales :: Integer -> String
sirvoParaNaturales n    | n == 0 = "Listo :D"
                        | otherwise = sirvoParaNaturales (n - 1)

sumaICCSMQ :: Integer -> Integer
sumaICCSMQ n    | n == 1 = 0
                | otherwise = auxICCSMQ n n

auxICCSMQ :: Integer -> Integer -> Integer
auxICCSMQ n m   | m == 1 = 1
                | n > m^2 && not (par m) = m + auxICCSMQ n (m - 2)
                | otherwise = auxICCSMQ n (m - 1)