module Tablas where

-- type Tabla a b = [(a,b)]
-- El type tiene el problema a nivel TAD deberíamos abstraer la especificacion de la implementacion
-- Poner una lista y exportarla es peligroso, porque podrían usarse las funciones de listas para trabajar sobre las tablas, lo cual puede no tener sentido
-- Por esto es apropiado usar un Data y definir un tipo de datos nuevo
-- De esta manera cuando importo el modulo desde otro archivo, no se puede usar el constructor pero si las funciones

data Tabla a b = T [(a,b)] deriving Show -- T es el constructor


empty :: Tabla a b
empty = T []

lkup :: Eq a => a -> Tabla a b -> Maybe b
lkup k (T t) = lookup k t --lookup del preludio

upd :: a -> b -> Tabla a b -> Tabla a b
upd k v (T t) = T ((k,v):t) -- inserta al principio, no busca el elemento para actualizar

del :: Eq a => a -> Tabla a b -> Tabla a b
del k (T t) = T (filter (\(k',v') -> k' /= k) t)     

getKeys :: Tabla a b -> [a]
getKeys (T t) = map fst t









