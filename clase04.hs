potencia :: Float -> Integer -> Float
potencia a n    | n == 0 = 1
                | n > 0 = a * potencia a (n - 1)

sumaICCSMQ :: Integer -> Integer
sumaICCSMQ n    | n <= 1 = 0
                | otherwise = auxICCSMQ 1 n

auxICCSMQ n m   | n^2 >= m = 0
                | otherwise = n + auxICCSMQ (n + 2) m

division :: Integer -> Integer -> (Integer, Integer)
division a d    | a < d = (0, a)
                | a >= d = (1 + fst (division (a - d) d), snd (division (a - d) d))

division2 :: Integer -> Integer -> (Integer, Integer)
division2 a d   | a < d = (0, a)
                | a >= d = (1 + fst qr', snd qr') where qr' = division (a - d) d

divParcial :: Integer -> Integer -> [Integer]
divParcial n m  | n == m = [n]
                | snd (division2 n m) == 0 = [m] ++ divParcial n (m + 1)
                | otherwise = divParcial n (m + 1)

divisores :: Integer -> [Integer]
divisores n = divParcial n 1

esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2

divParcial2 :: Integer -> Integer -> [Integer]
divParcial2 n m | m == 1 = [1]
                | mod n m == 0 = m : divParcial2 n (m - 1)
                | otherwise = divParcial2 n (m - 1)

divisores2 :: Integer -> [Integer]
divisores2 n = divParcial2 n n

esPrimo2 :: Integer -> Bool
esPrimo2 n = length (divisores2 n) == 2

productoria :: [Integer] -> Integer
productoria a   | length a == 1 = head a
                | otherwise = head a * productoria (tail a)

reverso :: [Integer] -> [Integer]
reverso a = reverse a

reverso2 :: [a] -> [a]
reverso2 a  | length a == 0 = []
            | otherwise = reverso2 (tail a) ++ [head a]

{-reverso ['a', 'b', 'c']	-> reverso (tail ['a', 'b', 'c']) ++ [head ['a', 'b', 'c']]
                            -> reverso ['b', 'c'] ++ [head ['a', 'b', 'c']]
							-> reverso (tail ['b', 'c']) ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> reverso ['c'] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> reverso (tail ['c']) ++ [head ['c']] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> reverso [] ++ [head ['c']] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> [] ++ [head ['c']] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> [] ++ ['c'] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> ['c'] ++ [head ['b', 'c']] ++ [head ['a', 'b', 'c']]
							-> ['c'] ++ ['b'] ++ [head ['a', 'b', 'c']]
							-> ['c', 'b'] ++ [head ['a', 'b', 'c']]
							-> ['c', 'b'] ++ ['a']
							-> ['c', 'b', 'a']-}

capicua :: [Integer] -> Bool
capicua a = a == reverso2 a

suma :: [Integer] -> [Integer] -> [Integer]
suma a b    | a == [] = b
            | b == [] = a
            | otherwise = (head a + head b) : suma (tail a) (tail b)

prodInterno :: [Float] -> [Float] -> Float
prodInterno a b | length a == 1 = head a * head b
                | otherwise = prodInterno (tail a) (tail b) + (head a * head b)

algoritmoDivision :: Integer -> Integer -> (Integer, Integer)
algoritmoDivision a d   | a >= 0 && (a < d || a < (-d)) = (0, a)
                        | a * d > 0 = (1 + fst qr'a, snd qr'a)
                        | a * d < 0 = ((-1) + fst qr'b, snd qr'b)
                        where (qr'a, qr'b) = (algoritmoDivision (a - d) d, algoritmoDivision (a + d) d)

algoritmoDivision2 :: Integer -> Integer -> (Integer, Integer)  --resultados a partir del caso a, d > 0
algoritmoDivision2 a d  | a < 0 && d < 0 = (fst (algoritmoDivision (-a) (-d)) + 1, (-d) - snd (algoritmoDivision (-a) (-d)))
                        | a < 0 && d > 0 = ((-1) * (fst (algoritmoDivision (-a) d)) - 1, d - snd (algoritmoDivision (-a) d))
                        | a > 0 && d < 0 = ((-1) * (fst (algoritmoDivision a (-d))), snd (algoritmoDivision a (-d)))
                        | otherwise = algoritmoDivision a d
                        
noTieneDivisoresHasta :: Integer -> Integer -> Bool             --recorre desde (n - 1) hasta 2
noTieneDivisoresHasta m n   | snd (algoritmoDivision n m) == 0 && m >= 2 = False
                            | m <= 2 = True
                            | otherwise = noTieneDivisoresHasta (m - 1) n

noTieneDivisoresHasta2 :: Integer -> Integer -> Bool            --recorre desde 2 hasta (n - 1)
noTieneDivisoresHasta2 m n  | snd (algoritmoDivision n m) == 0 = False
                            | m == n - 1 = True
                            | otherwise = noTieneDivisoresHasta2 (m + 1) n

esPrimoNTDH :: Integer -> Bool                                  --con mod en NTDH1/2 va mucho más rápido (modificar para NTDH2)
esPrimoNTDH n   | n == 1 = False
                | otherwise = noTieneDivisoresHasta (n - 1) n