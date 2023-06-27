pertenece :: Integer -> [Integer] -> Bool
pertenece a b   | b == [] = False
                | a == head b = True
                | otherwise = pertenece a (tail b)

hayRepetidos :: [Integer] -> Bool
hayRepetidos a  | a == [] = False
                | pertenece (head a) (tail a) == True = True
                | otherwise = hayRepetidos (tail a)

menores :: Integer -> [Integer] -> [Integer]
menores a b | b == [] = []
            | head b < a = head b : menores a (tail b)
            | otherwise = menores a (tail b)

quitar :: Integer -> [Integer] -> [Integer]
quitar a b  | b == [] = []
            | a == head b = tail b
            | a /= head b = head b : quitar a (tail b)

maximo :: [Integer] -> Integer
maximo a    | length a == 1 = head a
            | head a <= head (tail a) = maximo (tail a)
            | otherwise = maximo ([head a] ++ (tail (tail a)))

maximo2 :: [Integer] -> Integer                         --muy lento
maximo2 a   | length a == 1 = head a
            | length (menores (head a) a) == (length a) - 1 = head a
            | otherwise = maximo2 (tail a)

enBase :: Integer -> Integer -> [Integer]
enBase a b  | div a b == 0 = [a]
            | otherwise = enBase (div a b) b ++ [mod a b]

deBase :: Integer -> [Integer] -> Integer
deBase a b  | length b == 1 = head b
            | otherwise = a ^ ((length b) - 1) * (head b) + deBase a (tail b)

capicuaPara :: [Integer] -> [Integer]
capicuaPara a   | a == reverse a = a
                | otherwise = capicuaPara (enBase (deBase 10 a + deBase 10 (reverse a)) 10)

cambiodeBase :: Integer -> Integer -> [Integer] -> [Integer]
cambiodeBase b1 b2 a    | b1 == b2 = a
                        | otherwise = enBase (deBase b1 a) b2

ordenNoDecreciente :: [Integer] -> Bool
ordenNoDecreciente xl   | length xl == 1 = True
                        | head xl <= head (tail xl) = ordenNoDecreciente (tail xl)
                        | otherwise = False

sinRepeticionesA :: [Integer] -> [Integer]              --medio lento
sinRepeticionesA xl | xl == [] = []
                    | pertenece (head xl) (tail xl) == False = head xl : sinRepeticionesA (tail xl)
                    | otherwise = sinRepeticionesA (tail xl)

sinRepeticionesB :: [Integer] -> [Integer]              --lento
sinRepeticionesB xl = reverse (sinRepeticionesA (reverse xl))

unionNoDecreciente :: [Integer] -> [Integer] -> [Integer]
unionNoDecreciente xl yl    | xl == [] = yl
                            | yl == [] = xl
                            | head xl <= head yl = head xl : unionNoDecreciente (tail xl) yl
                            | otherwise = head yl : unionNoDecreciente xl (tail yl)

deMenoraMayor :: [Integer] -> [Integer]
deMenoraMayor xl    | length xl == 1 = [head xl]
                    | head xl <= minimum (tail xl) = head xl : deMenoraMayor (tail xl)
                    | otherwise = deMenoraMayor (tail xl ++ [head xl])