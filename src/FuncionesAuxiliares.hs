module FuncionesAuxiliares where
import Data.Function
import Data.List

on2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
on2 op f g x = f x `op` g x

-- compone una función que toma 2 parametros con otra que toma 1. Otra forma:
-- (.|..) f g x y = f (g x y)
(.|..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.|..) = (.) . (.)

padD :: a -> Int -> [a] -> [a]
padD a n s = s ++ replicate (n - length s) a
-- si n < 0, replicate n _ == []

-- se podria hacer todavia mas general con signatura
-- zipMaxWith' :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
-- pero creemos que no vale la pena, y asi evitamos repetir el caso base.
zipMaxWith :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipMaxWith f b xs ys = (zipWith f `on` padD b maxL) xs ys
                        where maxL = (max `on` length) xs ys

-- Devuelve el valor de la lista en el indice pasado como parámetro, y en caso
-- de out-of-bounds, devuelve el valor por default.
índiceODefault :: a -> [a] -> Int ->  a
índiceODefault a l ind = if ind >= length l then a else l !! ind
