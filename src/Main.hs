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
  3 ~=? capacidad puroContenedor,
  0 ~=? capacidad nave3,
  1 ~=? capacidad nave4,

  0 ~=? poderDeAtaque nave1,
  1 ~=? poderDeAtaque nave2,
  1 ~=? poderDeAtaque nave3,
  2 ~=? poderDeAtaque nave4,
  2 ~=? poderDeAtaque nave5,
  0 ~=? poderDeAtaque escudoSinCañon,

  False ~=? puedeVolar contenedorSolo,
  True ~=? puedeVolar nave1,
  True ~=? puedeVolar nave2,
  True ~=? puedeVolar nave3,
  True ~=? puedeVolar nave4,
  True ~=? puedeVolar nave5,
  True ~=? puedeVolar nave6,
  True ~=? puedeVolar nave7,
  False ~=? puedeVolar tresCañones,
  False ~=? puedeVolar puroContenedor,
  False ~=? puedeVolar contenedorYCañon,

  True ~=? mismoPotencial nave9 nave9,
  True ~=? mismoPotencial nave2 nave3,
  True ~=? mismoPotencial nave4 nave5,
  True ~=? mismoPotencial nave6 nave7,
  False ~=? mismoPotencial nave6 nave9,
  False ~=? mismoPotencial nave1 nave9
  ]

testsEj3 = test [
  nave1 ~=? mayorCapacidad [nave1],
  contenedorSolo ~=? mayorCapacidad [contenedorSolo, nave1, nave2, nave3],
  nave9 ~=? mayorCapacidad [contenedorSolo, nave1, nave2, nave3, nave4, nave5, nave6, nave9],
  puroContenedor ~=? mayorCapacidad [puroContenedor, contenedorYCañon]
  ]

rotarComponentes :: Componente -> Componente
rotarComponentes Contenedor = Motor
rotarComponentes Motor = Escudo
rotarComponentes Escudo = Cañón
rotarComponentes Cañón = Contenedor

testsEj4 = test [
  nave9 ~=? transformar id nave9,
  nave8 ~=? transformar id nave8,
  tresCañones ~=? transformar (const Cañón) puroContenedor,
  tresCañones ~=? transformar (const Cañón) escudoSinCañon,
  tresCañones ~=? transformar (const Cañón) puroContenedor,
  puroContenedor ~=? transformar rotarComponentes tresCañones,
  (Módulo Motor (Base Motor) (Base Contenedor)) ~=?
      transformar rotarComponentes otroCañon
  ]

-- destruye solo el subárbol izquierdo del subárbol izquierdo.
nave9postImpactar1 = Módulo Escudo
      (Módulo Escudo (Base Contenedor)
                     (Módulo Motor (Base Contenedor) (Base Motor)))
      (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor))
                     (Módulo Escudo (Base Cañón) (Base Escudo)))

-- destruye todo el subárbol izquierdo.
nave9postImpactar2 = Módulo Escudo
      (Base Contenedor)
      (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor))
                     (Módulo Escudo (Base Cañón) (Base Escudo)))

nave9postImpactar3 = Módulo Escudo
      (Base Contenedor)
      (Módulo Escudo (Módulo Contenedor (Base Contenedor) (Base Contenedor))
                     (Módulo Escudo (Base Cañón) (Base Escudo)))

nave9postImpactar4 = Módulo Escudo
      (Base Contenedor)
      (Módulo Escudo (Base Contenedor)
                     (Módulo Escudo (Base Cañón) (Base Escudo)))

nave9postImpactar5 = Módulo Escudo
      (Base Contenedor)
      (Módulo Escudo (Base Contenedor)
                     (Módulo Escudo (Base Contenedor) (Base Escudo)))


impacto1a, impacto1b, impacto2, impacto3a, impacto3b, impacto4, impacto5,
  impacto6, impacto7:: Peligro
impacto1a = (Babor, 0, Grande)
impacto1b = (Estribor,0,Pequeño)
impacto2 = (Babor,2,Torpedo)
impacto3a = (Estribor,0,Pequeño)
impacto3b = (Babor, 1, Grande)
impacto4 = (Babor, 3, Pequeño)
impacto5 = (Estribor, 2, Grande)
impacto6 = (Babor, 2, Pequeño)
impacto7 = (Babor, 3, Grande)

testsEj5 = test [
  nave9 ~=? impactar impacto1b nave9,
  nave9postImpactar1 ~=? impactar impacto2 nave9,
  nave9postImpactar2 ~=? impactar impacto3b nave9postImpactar1,
  nave9postImpactar3 ~=? impactar impacto4 nave9postImpactar2,
  nave9postImpactar3 ~=? impactar impacto5 nave9postImpactar3,
  nave9postImpactar4 ~=? impactar impacto6 nave9postImpactar3,
  nave9postImpactar5 ~=? impactar impacto7 nave9postImpactar4
  ]


testsEj6 = test [
  nave9postImpactar1 ~=? maniobrar nave9 [impacto1a, impacto2, impacto3a],
  nave9postImpactar2 ~=? maniobrar nave9 [impacto1b, impacto2, impacto3b]
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

  (1,1) ~=? dimensiones contenedorSolo,
  (1,1) ~=? dimensiones nave1,
  (2,2) ~=? dimensiones nave2,
  (3,4) ~=? dimensiones nave4,
  (4,4) ~=? dimensiones nave6,
  (5,4) ~=? dimensiones nave8,
  (4,8) ~=? dimensiones nave9,
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]
