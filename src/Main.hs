module Main where
import NavesEspaciales
import Test.HUnit
import Data.List



--Naves para pruebas:
contenedorSolo = Base Contenedor
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo
      (Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón))
                     (Módulo Motor (Base Contenedor) (Base Motor)))
      (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor))
                     (Módulo Escudo (Base Cañón) (Base Escudo)))

soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido

desbalanceado = Módulo Escudo (Base Contenedor) protegido


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
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  3 ~=? capacidad puroContenedor
  ]

testsEj3 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj5 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  [nave1,nave3,nave9] ~=?
         pruebaDeFuego
                 [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)]
                 [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9]
  ]

testsEj8 = test [
  1 ~=? (componentesPorNivel nave1 0),
  0 ~=? (componentesPorNivel nave1 1),

  1 ~=? (componentesPorNivel nave2 0),
  2 ~=? (componentesPorNivel nave2 1),
  0 ~=? (componentesPorNivel nave2 2),

  1 ~=? (componentesPorNivel nave3 0),
  2 ~=? (componentesPorNivel nave3 1),
  0 ~=? (componentesPorNivel nave3 2),

  1 ~=? (componentesPorNivel nave4 0),
  2 ~=? (componentesPorNivel nave4 1),
  4 ~=? (componentesPorNivel nave4 2),
  0 ~=? (componentesPorNivel nave4 3),

  1 ~=? (componentesPorNivel nave8 0),
  2 ~=? (componentesPorNivel nave8 1),
  2 ~=? (componentesPorNivel nave8 2),
  2 ~=? (componentesPorNivel nave8 3),
  4 ~=? (componentesPorNivel nave8 4),
  0 ~=? (componentesPorNivel nave8 5),

  1 ~=? (componentesPorNivel nave9 0),
  2 ~=? (componentesPorNivel nave9 1),
  4 ~=? (componentesPorNivel nave9 2),
  8 ~=? (componentesPorNivel nave9 3),
  0 ~=? (componentesPorNivel nave9 4),
  0 ~=? (componentesPorNivel nave9 5),

  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]


--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.
