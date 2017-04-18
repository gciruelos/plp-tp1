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

-- Derivamos Enum y Bounded para listar todos los valores posibles de Componente
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
-- f es la función que se aplica en el caso recursivo (Módulo) y g, en el caso base
foldNave :: (Componente -> b -> b -> b ) -> (Componente -> b)
         -> NaveEspacial -> b
foldNave f g (Módulo c n1 n2) = on (f c) (foldNave f g) n1 n2
foldNave f g (Base c) = g c

-- Utilidades que vamos a usar después.

-- zipMaxWith es equivalente a zipWith, pero continúa hasta alcanzar la mayor, en vez de la menor, de las dos longitudes
-- Al no poder tomar valores de la lista más corta, usa el default provisto (en este caso 0)
-- 
-- Devuelve una lista con la cantidad de componentes en cada nivel
-- En el caso base devuelve la lista [1], representando que esa nave tiene un solo componente
-- En el caso recursivo, suma la cantidad de componentes de los hijos, nivel a nivel, y les adjunta 1
--
-- El uso de const se debe a que no necesitamos el valor del componente
-- El operador f (.|..) g compone funciones tales que g toma dos argumentos y f uno
--
-- Se podría generalizar aún más, pero con devolver la cantidad de componentes en cada nivel nos alcanza para lo que necesitamos
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

-- Generamos la lista de cantidades de cada componente para ambas naves
-- y comprobamos que sean iguales
mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial = (==) `on` cantidadDeCadaComponente

-- Ejercicio 3

-- Obtenemos el máximo de acuerdo al orden de capacidad

-- Una idea similar sería maximum . map capacidad, pero el resultado
-- sería la capacidad máxima, no la nave de capacidad máxima

-- La desventaja del approach elegido es que se calcula nuevamente la 
-- capacidad en cada comparación; una forma más eficiente sería
-- mayorCapacidad = fst . maximumBy (compare `on` snd) . map (on2 (,) id cantidadDeCadaComponente)
-- Esta solución emplea la idea del map pero mantiene el valor original--  de la nave junto con su capacidad
-- Sin embargo, la versión presentada es más simple y fácil de entender
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = maximumBy (compare `on` capacidad)

-- Ejercicio 4

-- Generamos nuevamente la nave en cada nivel, aplicando la función al componente.
-- Misma idea que map f = foldr ((:) . f) []
transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (Módulo . f) (Base . f)

-- Ejercicio 5
protegidoPorCañón :: NaveEspacial -> Bool
protegidoPorCañón = (> 0) . poderDeAtaque

-- Notar que esto no es inducción estructural, si no simplemente una división
-- por casos no-recursiva.
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

-- Sólo se debe impactar al nivel más de la izquierda/derecha (dependiendo de
-- la dirección del Peligro); desde un cierto nodo del árbol no podemos conocer
-- más que sus hijos, por lo que no podemos saber si se debe impactar ahí o no.
-- Al realizar recursión explícita, podemos asegurarnos que siempre estamos
-- bajando desde la raíz siempre por la rama que contiene al nodo a impactar.
-- Al foldear esto no es posible, ya que se realiza de una forma "bottom-up"
-- sin poder acceder al padre de un nodo,  ni comprobar si un nodo es la raíz
-- del árbol.
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (_, 0, t) (Base c) = Base
                        (if t == Pequeño && c == Escudo then c else Contenedor)
impactar (_, nivel, t) (Base c) = Base c
impactar (_, 0, t) m@(Módulo c n1 n2) = resultadoDelImpacto t m
impactar (d, nivel, t) (Módulo c n1 n2) =
        --altura (Base c) == 1, por eso comparamos con nivel en vez de nivel - 1
        if atacoIzquierda d nivel n1 n2
        then Módulo c (impactar (d, nivel - 1, t) n1) n2
		else Módulo c n1 (impactar (d, nivel - 1, t) n2)

atacoIzquierda :: Dirección -> Int -> NaveEspacial -> NaveEspacial -> Bool
atacoIzquierda Babor nivel n1 n2 = altura n1 >= nivel
atacoIzquierda Estribor nivel n1 n2 = altura n2 < nivel

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

-- componentesEnCadaNivel nos da exactamente lo que buscamos, pero
-- para todos los niveles. Simplemente indexamos la lista en el
-- nivel deseado

-- El único detalle es que podrían pedirse los componentes de un nivel
-- mayor al máximo de la nave; en dicho caso devolvemos 0.
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel n = índiceODefault 0 $ componentesEnCadaNivel n

-- La altura no es más que la longitud de la lista de 
-- componentesEnCadaNivel, y el ancho que el valor máximo
-- Como ya teníamos definida la altura antes, podríamos 
-- realizar la siguiente implementación
-- dimensiones = on2 ( , ) altura (maximum . componentesEnCadaNivel)
-- Esta definición evita "reinventar la rueda" con altura, pero quizás 
-- reduce un poco la claridad de la definición
dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = (on2 (,) length maximum) . componentesEnCadaNivel
