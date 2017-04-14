module FuncionesAuxiliares where 
import Data.Function
import Data.List

on2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
on2 op f g x = f x `op` g x

(.|..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.|..) = (.) . (.)

--se podria hacer todavia mas general con signatura 
--zipMaxWith' :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
--pero creo que no vale la pena, y asi evitamos repetir el caso base
zipMaxWith :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipMaxWith f b xs ys = (zipWith f `on` pad b maxL) xs ys
                        where maxL = (max `on` length) xs ys
                              pad b l = on2 (++) id $ unfoldr (\ys -> if length ys >= l then Nothing else Just(b, b:ys))
