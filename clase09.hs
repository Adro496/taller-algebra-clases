data Polinomio = Mono Float Integer
                | Suma Polinomio Polinomio
                | Producto Polinomio Polinomio

{-  3x⁴ = Mono 3 4
    7 = Mono 7 0
    2x + 7 = suma (Mono 2 1) (Mono 7 0)
    (x² + 2)(x + 3) = Producto (suma (Mono 1 2) (Mono 2 0)) (suma (Mono 1 1) (Mono 3 0))    -}

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) x = a * x ^ n
evaluar (Suma p q) x = evaluar p x + evaluar q x
evaluar (Producto p q) x = evaluar p x * evaluar q x

coeficientes :: Polinomio -> [Float]
coeficientes (Mono 0 _) = []
coeficientes (Mono a n) | n == 0 = [a]
                        | otherwise = 0 : coeficientes (Mono a (n - 1))

coeficientes (Suma p q) = sumarCoeficientes (coeficientes p) (coeficientes q)
coeficientes (Producto p q) = multiplicar (coeficientes p) (coeficientes q)

sumarCoeficientes :: [Float] -> [Float] -> [Float]
sumarCoeficientes a b   | a == [] = b
                        | b == [] = a
                        | otherwise = (head a + head b) : sumarCoeficientes (tail a) (tail b)

multiplicar :: [Float] -> [Float] -> [Float]
multiplicar xl yl   | xl == [] || yl == [] = []
                    | otherwise = sumarCoeficientes (auxMult (head xl) yl) (0 : multiplicar (tail xl) yl)

auxMult :: Float -> [Float] -> [Float]
auxMult x xl    | xl == [] = []
                | otherwise = (x * head xl) : auxMult x (tail xl)

derivada :: Polinomio -> Polinomio
derivada p = auxDeriv (coeficientes p) 0

auxDeriv :: [Float] -> Integer -> Polinomio
auxDeriv xl n   | n == 0 || head xl == 0 = auxDeriv (tail xl) (n + 1)
                | length xl == 1 = Mono (x * head xl) (n - 1)
                | otherwise = Suma (Mono (x * head xl) (n - 1)) (auxDeriv (tail xl) (n + 1))
                where x = realToFrac n

instance Num Polinomio where
    (+) p q = Suma p q
    (*) p q = Producto p q
    negate (Mono x n) = Mono (-x) n
    negate (Suma p q) = Suma (-p) (-q)
    negate (Producto p q) = Producto (-p) q
    fromInteger n = Mono (realToFrac n) 0
    abs = undefined
    signum = undefined

instance Show Polinomio where
    show p = polToString (coeficientes p) 0

polToString :: [Float] -> Integer -> String
polToString xl n    | xl == [] = show 0
                    | length xl == 1 = show (head xl) ++ "x^" ++ show n
                    | head xl == 0 = polToString (tail xl) (n + 1)
                    | otherwise = show (head xl) ++ "x^" ++ show n ++ " + " ++ polToString (tail xl) (n + 1)