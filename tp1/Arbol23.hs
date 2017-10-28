module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b)
                 | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5

padTree :: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b) -> String
padTree nivel acum doPad t = case t of
        (Hoja x) -> initialPad ++ stuff x
        (Dos x i d) -> initialPad ++ stuff x ++
                pad padlength ++ rec x False i ++ "\n" ++
                rec x True d ++ "\n"
        (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                pad padlength ++ rec x False i ++ "\n" ++
                pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                rec x True d ++ "\n"
    where l = length . stuff
          levelPad = (padlength*nivel + acum)
          initialPad = (if doPad then pad levelPad else "")
          rec x = padTree (nivel+1) (acum+l x)

stuff :: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
    where s = show x
          l = length s
          n = padlength

pad :: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23 :: (a->c) -> (b->c->c->c) -> (b->b->c->c->c->c) -> Arbol23 a b -> c
foldA23 f g h a = case a of
                    (Hoja x) -> f x
                    (Dos x t1 t2) -> g x (fld t1) (fld t2)
                    (Tres x y t1 t2 t3) -> h x y (fld t1) (fld t2) (fld t3)
    where fld = foldA23 f g h

--Lista en preorden de los internos del árbol.
    --
    -- Componemos funciones que construyen listas en vez de concatenarlas
    -- explícitamente pues la concatenación tarda tiempo lineal en el tamaño
    -- del primer argumento (tiene que reconstruir la lista),
    -- por lo que `internos` tardaría tiempo cuadrático en la cantidad de nodos
    --
    -- En cambio si componemos funciones y recién las evaluamos al final
    -- podemos hacer todo el proceso en tiempo lineal
internos :: Arbol23 a b -> [b]
internos arbol = foldA23 (const id)
                         (\x r1 r2 -> (x:) . r1 . r2)
                         (\x y r1 r2 r3 -> (x:) . (y:) . r1 . r2 . r3) arbol []

--Lista las hojas de izquierda a derecha.
    -- Ver el comentario de `internos`
hojas :: Arbol23 a b -> [a]
hojas a = foldA23 (:) (\x r1 r2 -> r1 . r2) (\x y r1 r2 r3 -> r1 . r2 . r3) a []

esHoja :: Arbol23 a b -> Bool
esHoja = foldA23 (const True) (\x r1 r2 -> False) (\x y r1 r2 r3 -> False)

mapA23 :: (a -> c) -> (b -> d) -> Arbol23 a b -> Arbol23 c d
mapA23 fa fb = foldA23 (Hoja . fa)
                       (Dos . fb)
                       (\x y -> Tres (fb x) (fb y))


--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas :: Num a => Arbol23 a b -> Arbol23 a b
incrementarHojas = mapA23 (+1) id


--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
truncar :: a -> Integer -> Arbol23 a b -> Arbol23 a b
truncar h n t = foldA23 fnHoja fnDos fnTres t n
    where cutAt i t = if i == 0 then Hoja h else t
          fnHoja x i = cutAt i $ Hoja x
          fnDos x r1 r2 i = cutAt i $ Dos x (r1 $ i-1) (r2 $ i-1)
          fnTres x y r1 r2 r3 i = cutAt i $ Tres x y (r1 $ i-1) (r2 $ i-1) (r3 $ i-1)

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar :: Arbol23 a (a->a->a) -> a
evaluar = foldA23 id id (\f g t1 t2 t3 -> g (f t1 t2) t3)

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1 :: Arbol23 Char Int
arbolito1 = Tres 0 1
            (Dos 2 (Hoja 'a') (Hoja 'b'))
            (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
            (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2 :: Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3 :: Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4 :: Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12)))
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))

