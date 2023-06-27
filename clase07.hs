data Direccion = Norte | Sur | Este | Oeste deriving Show
type Tortuga = (Pos, Direccion)
type Pos = (Integer, Integer)

arranca :: Tortuga
arranca = ((0,0), Norte)

girarDerecha :: Tortuga -> Tortuga
girarDerecha (p, Norte) = (p, Este)
girarDerecha (p, Este) = (p, Sur)
girarDerecha (p, Sur) = (p, Oeste)
girarDerecha (p, Oeste) = (p, Norte)

avanzar :: Tortuga -> Integer -> Tortuga
avanzar ((x, y), Norte) n = ((x, y + n), Norte)
avanzar ((x, y), Este) n = ((x + n, y), Este)
avanzar ((x, y), Sur) n = ((x, y - n), Sur)
avanzar ((x, y), Oeste) n = ((x - n, y), Oeste)

data Figura = Rectangulo Float Float Float Float
            | Circulo Float Float Float deriving Show

c1 :: Figura
c1 = Circulo 0 0 pi

r1 :: Float -> Figura
r1 x = Rectangulo 0 0 (x / sqrt 2) (x / sqrt 2)

area :: Figura -> Float
area (Rectangulo x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)
area (Circulo x y r) = pi * r^2

data Punto = Point Float Float deriving Show
data Figura2 = Rectangulo2 Punto Punto | Circulo2 Punto Float deriving Show

area2 :: Figura2 -> Float
area2 (Rectangulo2 (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)
area2 (Circulo2 p r) = pi * r^2

data ProgAritmetica = Vacio | CongruentesA Integer Integer

esMultiplo :: Integer -> Integer -> Bool
esMultiplo a b = mod a b == 0

pertenece :: Integer -> ProgAritmetica -> Bool
pertenece k Vacio = False
pertenece k (CongruentesA n m) = esMultiplo (k - n) m

incluido :: ProgAritmetica -> ProgAritmetica -> Bool    --(n1 + m1) pertenece a Congruentes n2 m2 implica la inclusión?
incluido Vacio _ = True
incluido _ Vacio = False
incluido (CongruentesA n1 m1) (CongruentesA n2 m2) = pertenece n1 (CongruentesA n2 m2) && esMultiplo m1 m2

instance Show ProgAritmetica where
    show Vacio = "{}"
    show (CongruentesA x d) = "{a en Z | a = " ++ show x ++ " (mod " ++ show d ++ ")}"

suma :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
suma (CongruentesA x1 d1) (CongruentesA x2 d2) = CongruentesA (x1 + x2) (gcd d1 d2)

interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica {-| not (esMultiplo d2 d1 || esMultiplo d1 d2) = CongruentesA auxInt mcm-}
interseccion Vacio _ = Vacio
interseccion _ Vacio = Vacio
interseccion (CongruentesA x1 d1) (CongruentesA x2 d2)  | incluido p q = CongruentesA x1 d1
                                                        | incluido q p = CongruentesA x2 d2
                                                        | auxInt > 0 = CongruentesA auxInt mcm
                                                        | otherwise = Vacio
    where (p, q, mcm, auxInt) = (CongruentesA x1 d1, CongruentesA x2 d2, lcm d1 d2, positivoSiHayInterseccion p q mcm)

positivoSiHayInterseccion p q mcm   | mcm == 0 = 0 --se podría mejorar la eficiencia haciendo que busque desde x1 hasta (mcm + x1) en intervalos de d1
                                    | pertenece mcm p && pertenece mcm q = mcm
                                    | otherwise = positivoSiHayInterseccion p q (mcm - 1)

interseccion2 :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
interseccion2 Vacio _ = Vacio
interseccion2 _ Vacio = Vacio
interseccion2 (CongruentesA x1 d1) (CongruentesA x2 d2) | incluido p q = CongruentesA x1 d1
                                                        | incluido q p = CongruentesA x2 d2
                                                        | auxInt > 0 = CongruentesA auxInt mcm
                                                        | otherwise = Vacio
    where (p, q, mcm, auxInt) = (CongruentesA x1 d1, CongruentesA x2 d2, lcm d1 d2, restoSiHayInt p q x1)

restoSiHayInt (CongruentesA x1 d1) (CongruentesA x2 d2) xi  | xi > x1 + lcm d1 d2 = -1 --se podría mejorar haciendo que use la progresión de mayor módulo
                                                            | pertenece xi p && pertenece xi q = xi
                                                            | otherwise = restoSiHayInt p q (xi + d1)                                    
    where (p, q) = (CongruentesA x1 d1, CongruentesA x2 d2)

iguales :: ProgAritmetica -> ProgAritmetica -> Bool
iguales p q = incluido p q && incluido q p

instance Eq ProgAritmetica where
    p == q = iguales p q

tieneSolucion :: Integer -> ProgAritmetica -> Bool
tieneSolucion k (CongruentesA c m) = esMultiplo c (gcd k m)

--volver a hacer los de congruencia cuando tenga claro el tema.