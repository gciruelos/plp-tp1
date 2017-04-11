module FuncionesAuxiliares where 

on2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
on2 op f g x = f x `op` g x

(.|..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.|..) f g = \x y -> f $ g x y

--se podria hacer todavia mas general con signatura 
--zipMaxWith' :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
--pero creo que no vale la pena, y asi me salvo de repetir el caso base
zipMaxWith :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipMaxWith f b [] [] = []
zipMaxWith f b [] (y:ys) = f b y : zipMaxWith f b [] ys
zipMaxWith f b (x:xs) [] = f x b : zipMaxWith f b xs [] 
zipMaxWith f b (x:xs) (y:ys) = f x y : zipMaxWith f b xs ys
