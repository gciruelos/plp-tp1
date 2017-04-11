module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where
import Data.Function
import Data.List

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show, Enum, Bounded)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . padNave 0 0 False
  
padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
					  pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
					  padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldNave :: (Componente -> b -> b -> b ) -> (Componente -> b) -> NaveEspacial -> b
foldNave f g (Módulo c n1 n2) = f c (foldNave f g n1) (foldNave f g n2)
foldNave f g (Base c) = g c

--Ejercicio 2
esComponente :: Componente -> Componente -> Int
esComponente c1 c2 = if c1 == c2 then 1 else 0

cantidadDeComponentes :: Componente -> NaveEspacial -> Int
cantidadDeComponentes c = foldNave (\modulo r1 r2 -> esElComponente modulo + r1 + r2) esElComponente
													where esElComponente = esComponente c

capacidad :: NaveEspacial -> Int
capacidad = cantidadDeComponentes Contenedor

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = cantidadDeComponentes Cañón

puedeVolar :: NaveEspacial -> Bool
puedeVolar = (> 0) . cantidadDeComponentes Motor

cantidadDeCadaComponente :: NaveEspacial -> [Int]
cantidadDeCadaComponente n = map (`cantidadDeComponentes` n) $ enumFrom (minBound :: Componente)

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial = (==) `on` cantidadDeCadaComponente

--Ejercicio 3
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = maximumBy (compare `on` cantidadDeComponentes Contenedor)

--Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (\modulo r1 r2 -> Módulo (f modulo) r1 r2) (Base . f)

-- Ejercicio 5
protegidoPorCañón = (> 0) . poderDeAtaque

haceDaño :: Peligro -> NaveEspacial -> Bool
--haceDaño (_, _, Pequeño) = 
-- TODO: agregar comentario porque no usamos fold
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (_, 0, t) (Base c) = Base (if t == Pequeño `and` c == Escudo then c else Contenedor)
impactar (_, nivel, t) (Base c) = Base c
--impactar (_, 0, t) n@(Módulo c n1 n2) = if protegidoPorCañon n `and`
impactar (Babor, nivel, t) (Módulo c n1 n2) = Módulo c (impactar (Babor, nivel - 1, t)  n1) n2
impactar (Estribor, nivel, t) (Módulo c n1 n2) = Módulo c n1 (impactar (Estribor, nivel - 1, t) n2)

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = undefined

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = undefined

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = undefined

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = undefined
