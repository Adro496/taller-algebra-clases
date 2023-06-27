data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol

{-  Hoja 10 :: Arbol
    Ramificacion (Hoja 20) 10 (Hoja 30) :: Arbol
    [Ramificacion (Hoja 2) 5 (Ramificacion (Hoja 1) 10 (Hoja 0)), Hoja 0] :: [Arbol]
    Ramificacion (Hoja 3) 5 (Ramificacion (Hoja 1)) :: Faltan parámetros     -}

esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja k) = k
sumaNodos (Ramificacion a1 k a2) = sumaNodos a1 + sumaNodos a2 + k

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Ramificacion a1 k a2) = 1 + max (altura a1) (altura a2)

pertenece :: Integer -> Arbol -> Bool
pertenece k (Hoja l) = k == l
pertenece k (Ramificacion a1 l a2)  | k == l = True
                                    | pertenece k a1 == True = True
                                    | otherwise = pertenece k a2

data Dir = Der | Izq

busqueda :: [Dir] -> Arbol -> Integer
busqueda [] (Hoja k) = k
busqueda [] (Ramificacion _ k _) = k
busqueda (Der:r) (Ramificacion _ _ a2) = busqueda r a2
busqueda (Izq:r) (Ramificacion a1 _ _) = busqueda r a1

data ArbolG a = HojaG a | Ramif (ArbolG a) a (ArbolG a) deriving Show

esHojaG :: ArbolG a -> Bool
esHojaG (HojaG _) = True
esHojaG _ = False

maximo :: Ord a => ArbolG a -> a
maximo (HojaG x) = x
maximo (Ramif a1 x a2)  | x >= max mx1 mx2 = x
                        | otherwise = max mx1 mx2
                        where (mx1, mx2) = (maximo a1, maximo a2)

raiz :: ArbolG a -> a
raiz (HojaG x) = x
raiz (Ramif _ x _) = x

todosIguales :: Eq a => ArbolG a -> Bool
todosIguales (HojaG _) = True
todosIguales (Ramif a1 x a2)    | (x, x) == (raiz a1, raiz a2) = todosIguales a1 && todosIguales a2
                                | otherwise = False

espejar :: ArbolG a -> ArbolG a
espejar (HojaG x) = HojaG x
espejar (Ramif a1 x a2) = Ramif (espejar a2) x (espejar a1)

esHeap :: Ord a => ArbolG a -> Bool
esHeap (HojaG _) = True
esHeap (Ramif a1 x a2)  | x <= raiz a1 && x <= raiz a2 = esHeap a1 && esHeap a2
                        | otherwise = False

data Lista a = Vacia | Agregar a (Lista a)

vacia :: Lista a -> Bool
vacia Vacia = True
vacia _ = False

suma :: Lista Float -> Float
suma Vacia = 0
suma (Agregar x xl) = x + suma xl

enPosicion :: Lista a -> Integer -> a
enPosicion (Agregar e xl) 1 = e
enPosicion (Agregar e xl) n = enPosicion xl (n - 1)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales Vacia Vacia = True
iguales Vacia _ = False
iguales _ Vacia = False
iguales (Agregar e xl) (Agregar f yl) = (e, iguales xl yl) == (f, True)

juntar :: Lista a -> Lista b -> Lista (a, b)
juntar Vacia Vacia = Vacia
juntar (Agregar a xl) (Agregar b yl) = Agregar (a, b) (juntar xl yl)

instance Show a => Show (Lista a) where
    show Vacia = "[]"
    show (Agregar a Vacia) = "[" ++ show a ++ "]"
    show (Agregar a xl) = "[" ++ show a ++ "," ++ tail (show xl)