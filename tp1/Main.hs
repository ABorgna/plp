import Diccionario
import Data.List
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

búsquedaDelTesoro :: Eq a => a -> (a -> Bool) -> Diccionario a a -> Maybe a
búsquedaDelTesoro = undefined

{- Diccionarios de prueba: -}

dicc1 :: Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")]
                      (vacio (<))

dicc2 :: Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),
                       ("casa","escalera"),("ropero","alfajor"),("escalera","ropero")]
                       (vacio (<))

dicc3 :: Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")]
                      (vacio (\x y->x `mod` 5 < y `mod` 5))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
    "ejercicio2" ~: testsEj2,
    "ejercicio3" ~: testsEj3,
    "ejercicio4" ~: testsEj4,
    "ejercicio5" ~: testsEj5,
    "ejercicio6" ~: testsEj6,
    "ejercicio7" ~: testsEj7,
    "ejercicio8" ~: testsEj8,
    "ejercicio9" ~: testsEj9,
    "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
    [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
    "abcdefghi" ~=? hojas arbolito1,
    [True,False,True] ~=? internos arbolito2,
    [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3)
  ]

testsEj3 = test [
    [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
    [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3)
  ]

testsEj5 = test [
    22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
    True ~=? (isNothing . estructura) (vacio (<) :: Diccionario Int Int)
  ]

testsEj7 = test [
     0 ~=? (length . concat . fmap hojas . estructura) (vacio (<) :: Diccionario Int Int),
     5 ~=? (length . concat . fmap hojas . estructura) dicc1,
     6 ~=? (length . concat . fmap hojas . estructura . definir 1 "Hi") dicc1
  ]

testsEj8 = test [
    Nothing ~=? obtener 42 (vacio (<) :: Diccionario Int Int),

    Nothing ~=? obtener 1 dicc1,
    Just "a" ~=? obtener 9 dicc1,
    Just "Hi" ~=? (obtener 1 . definir 1 "Hi") dicc1,

    Just "auto" ~=? obtener "calle" dicc2,

    Nothing ~=? obtener 4 dicc3,
    Just "a" ~=? obtener 9 dicc3,
    Just "Hi" ~=? (obtener 4 . definir 4 "Hi") dicc3
  ]

testsEj9 = test [
    [] ~=? claves (vacio (<) :: Diccionario Int Int),

    sort [0, -10, 15, 2, 9] ~=? (sort . claves) dicc1,
    sort [0, -10, 15, 2, 9, 1] ~=? (sort . claves . definir 1 "Hi") dicc1,

    sort ["inicio", "auto", "calle", "casa", "ropero", "escalera"]
            ~=? (sort . claves) dicc2
  ]

testsEj10 = test [
    Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2
  ]

