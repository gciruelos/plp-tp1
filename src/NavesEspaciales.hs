module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón),
                        NaveEspacial(Módulo, Base),
                        Dirección(Babor, Estribor),
                        TipoPeligro(Pequeño, Grande, Torpedo),
                        Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar,
                        mismoPotencial, mayorCapacidad, transformar, impactar,
                        maniobrar, pruebaDeFuego, componentesPorNivel,
                        dimensiones) where

import FuncionesAuxiliares
import Data.Function
import Data.List

data Componente = Contenedor | Motor | Escudo | Cañón
                deriving (Eq, Show, Enum, Bounded)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial
                  | Base Componente
                  deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . padNave 0 0 False

padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "")
                                    ++ show c
padNave nivel acum doPad (Módulo x i d) =
                          (if doPad then pad (4*nivel + acum) else "")
                          ++ show x
                          ++ pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n"
                          ++ padNave (nivel+1) (acum+l) True d
                          where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

-- Recordar `on`, es una función que vamos a usar seguido:
--   on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
--   on f g x y = f (g x) (g y)

-- Ejercicio 1
foldNave :: (Componente -> b -> b -> b ) -> (Componente -> b)
         -> NaveEspacial -> b
foldNave f g (Módulo c n1 n2) = on (f c) (foldNave f g) n1 n2
foldNave f g (Base c) = g c

-- Utilidades que vamos a usar después.
componentesEnCadaNivel :: NaveEspacial -> [Int]
componentesEnCadaNivel = foldNave (const $ (1 :) .|.. zipMaxWith (+) 0)
                                  (const [1])

altura = length . componentesEnCadaNivel

-- Ejercicio 2
-- Devolvemos Int en vez de Bool por un tema de conveniencia.
esComponente :: Componente -> Componente -> Int
esComponente c1 c2 = if c1 == c2 then 1 else 0

-- Para (Base c) hay que chequear que c sea el componente, y para
-- (Modulo c n1 n2) hay que chequear que c sea el componente y sumar las
-- llamadas recursivas de n1 y n2.
cantidadDeComponentes :: Componente -> NaveEspacial -> Int
cantidadDeComponentes c = foldNave
                            (\modulo r1 r2 -> esElComponente modulo + r1 + r2)
                            esElComponente
                          where esElComponente = esComponente c

capacidad :: NaveEspacial -> Int
capacidad = cantidadDeComponentes Contenedor

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = cantidadDeComponentes Cañón

puedeVolar :: NaveEspacial -> Bool
puedeVolar = (> 0) . cantidadDeComponentes Motor

-- Para cada componente (enumFrom (minBound :: Componente)), llamamos a
-- cantidadDeComponentes para la nave.
cantidadDeCadaComponente :: NaveEspacial -> [Int]
cantidadDeCadaComponente n = map (\c -> cantidadDeComponentes c n) $
                                     enumFrom (minBound :: Componente)

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial = (==) `on` cantidadDeCadaComponente

-- Ejercicio 3
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = maximumBy (compare `on` capacidad)

-- Ejercicio 4
transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (Módulo . f) (Base . f)

-- Ejercicio 5
protegidoPorCañón :: NaveEspacial -> Bool
protegidoPorCañón = (> 0) . poderDeAtaque

-- Notar que esto no es inducción estructural, si no simplemente una división
-- por casos.
raízEsEscudo :: NaveEspacial -> Bool
raízEsEscudo (Base c) = c == Escudo
raízEsEscudo (Módulo c _ _) = c == Escudo

-- La sub-nave después del impacto: Base Contenedor.
postImpacto :: NaveEspacial
postImpacto = Base Contenedor

resultadoDelImpacto :: TipoPeligro -> NaveEspacial -> NaveEspacial
resultadoDelImpacto Pequeño n = if raízEsEscudo n then n else postImpacto
resultadoDelImpacto Grande n = if raízEsEscudo n && protegidoPorCañón n
                               then n else postImpacto
resultadoDelImpacto Torpedo _ = postImpacto

-- TODO: agregar comentario porque no usamos fold
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (_, 0, t) (Base c) = Base
                        (if t == Pequeño && c == Escudo then c else Contenedor)
impactar (_, nivel, t) (Base c) = Base c
impactar (_, 0, t) m@(Módulo c n1 n2) = resultadoDelImpacto t m
impactar (Babor, nivel, t) (Módulo c n1 n2) =
        --altura (Base c) == 1, por eso comparamos con nivel en vez de nivel - 1
        if altura n1 >= nivel
        then Módulo c (impactar (Babor, nivel - 1, t) n1) n2
        else Módulo c n1 (impactar (Babor, nivel - 1, t) n2)
impactar (Estribor, nivel, t) (Módulo c n1 n2) =
        if altura n2 >= nivel
        then Módulo c n1 (impactar (Estribor, nivel - 1, t) n2)
        else Módulo c (impactar (Estribor, nivel - 1, t) n1) n2

-- Ejercicio 6
-- Es importante que sea foldl así la operación se asocia de la siguiente
-- manera:
--   maniobrar nave [p1, ..., pn] =
--     impactar (... (impactar p1 nave) ...) pn
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar nave = foldl (flip impactar) nave

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego peligros = filter (puedeVolar . (flip maniobrar peligros))

-- Ejercicio 8

componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel n = índiceODefault 0 $ componentesEnCadaNivel n

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = on2 ( , ) altura (maximum . componentesEnCadaNivel)
