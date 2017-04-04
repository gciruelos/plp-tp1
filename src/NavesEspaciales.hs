module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)
  
padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
					  pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
					  padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldNave :: undefined
foldNave = undefined

--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad = undefined

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = undefined

puedeVolar :: NaveEspacial -> Bool
puedeVolar = undefined

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial = undefined

--Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = undefined

--Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar = undefined

-- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar = undefined

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
