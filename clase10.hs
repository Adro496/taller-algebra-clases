data Rosetree t = Rose t [Rosetree t]

r1 = Rose 47 []

r2 = Rose 'a' [Rose 'x' [], Rose 'z' []]

r3 = Rose True [Rose False [Rose True [], Rose True [], Rose True []], Rose True [Rose False []]]

raiz :: Rosetree t -> t
raiz (Rose r _) = r

hijos :: Rosetree t -> [Rosetree t]
hijos (Rosetree _ hs) = hs

sumarTodo :: Num t => Rosetree t -> t
sumarTodo (Rose t []) = t
sumarTodo (Rose t hs) = t + sumarTodoListas hs

sumarTodoListas :: [Rosetree t] -> t
sumarTodoListas x:[] = sumarTodo x
sumarTodoListas x:xs = sumarTodo x + sumarTodoListas xs

{-	sumarTodo :: Num t => Rosetree t -> t
sumarTodo (Rose t []) = t
sumarTodo (Rose t hs) = t + sumarTodoListas hs

sumarTodoListas :: Num t => [Rosetree t] -> t
sumarTodoListas [Rose t []] = 0
sumarTodoListas [Rose t hs] = t + sumarTodo (head hs) + sumarTodoListas (tail hs)	-}

hojas :: Rosetree t -> [t]
hojas ()
hojas (Rose r hs) = hojasDeTodos hs

hojasDeTodos :: [Rosetree t] -> [t]
hojasDeTodos [] = []
hojasDeTodos x:xs = (hojas x) ++ (hojasDeTodos xs)

--funcion (Rose r hs) = r + sum (map funcion hs)