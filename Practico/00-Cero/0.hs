{-# OPTIONS_GHC -fno-warn-tabs #-}

import Prelude

data ABB a = Hoja | Nodo (ABB a) a (ABB a) deriving Show

-- not con λ − notation
notA :: Bool -> Bool
notA = \b -> case b of {True -> False; False -> True}

-- not con Pattern Matching
notP :: Bool -> Bool
notP True = False
notP False = True

-- (b) (&&) :: Bool -> Bool -> Bool
andA :: Bool -> Bool -> Bool
andA = \b1 -> \b2 -> case b1 of {True -> b2; False -> False}

andP :: Bool -> Bool -> Bool
andP True b2 = b2
andP False _ = False

-- (c) (kk) :: Bool -> Bool -> Bool

orA :: Bool -> Bool -> Bool
orA  = \b1 -> \b2 -> case b1 of {True -> True; False -> b2}

orP :: Bool -> Bool -> Bool
orP True _ = True
orP False b2 = b2

-- (d) (>>) :: Bool -> Bool -> Bool
-- ??

-- (e) (==) :: Bool -> Bool -> Bool
igualA :: Bool -> Bool -> Bool
igualA = \b1 b2 -> case b1 of
    True  -> case b2 of { True -> True;  False -> False }
    False -> case b2 of { True -> False; False -> True  }


igualP :: Bool -> Bool -> Bool
igualP True True = True
igualP True False = False
igualP False True = False
igualP False False = True

-- Integer:
-- (a) λ − notation y case:
sumiA:: Integer -> Integer
sumiA = \n -> case n of { 0 -> 0; x -> x+(sumiA (x-1))}

sumiP:: Integer -> Integer
sumiP 0 = 0
sumiP x = x+(sumiP (x-1))


-- (b) sumpi :: (Integer -> Bool) -> Integer -> Integer
-- Ej. sumpi even 8 = 0 + 2 + 4 + 6 + 8 = 20

sumpiA :: (Integer -> Bool) -> Integer -> Integer
sumpiA = \p n ->
    case n < 0 of
        True  -> 0
        False ->
            case p n of
                True  -> n + sumpiA p (n - 1)
                False -> sumpiA p (n - 1)

sumpiP :: (Integer -> Bool) -> Integer -> Integer
sumpiP p n
    | n < 0     = 0
    | p n       = n + sumpiP p (n - 1)
    | otherwise = sumpiP p (n - 1)

-- Recibe el divisor y devuleve una funcion que puede usarse como predicado
esMultiploDe :: Integer -> (Integer -> Bool)
esMultiploDe n = \x -> x `mod` n == 0

-- ej. sumpiP (esMultiploDe 5) 20 da como resultado 50 (0+5+10+15+20)

-- (d) sumpfi :: (Integer -> Bool) -> (Integer -> Integer) -> Integer -> Integer
-- Ej. sumpfi even (*2) 5 = 0*2 + 2*2 + 4*2 = 12


sumpfi :: (Integer -> Bool) -> (Integer -> Integer) -> Integer -> Integer
sumpfi p f n
    | n == 0    = if p 0 then f 0 else 0
    | n > 0     = (if p n then f n else 0) + sumpfi p f (n - 1)
    | otherwise = (if p n then f n else 0) + sumpfi p f (n + 1) -- para n negativos


-- Ej: funcion de integer a integer
suma1 :: Integer -> Integer
suma1 n = n+1

por2 :: Integer -> Integer
por2 n = n * 2

-- Listas

-- data [a] = [] | (:) a [a] //No se puede definir asi porque ya existe en Haskell

-- (a) λ − notation y case:
lengthA:: [a] -> Integer
lengthA = \l -> case l of { [] -> 0; x:xs -> 1+(lengthA xs)}
-- Pattern-Matching:
lengthP:: [a] -> Integer
lengthP [] = 0
lengthP (x:xs) = 1+(lengthP xs)


--(b) map :: (a -> b) -> [a] -> [b]
-- Ej. map even [1,4,3,0] = [False, True, False, True]

mapA :: (a -> b) -> [a] -> [b]
mapA _ [] = []
mapA f (x:xs) = f x : (mapA f xs)

-- Arboles

insert :: Ord a => a -> ABB a -> ABB a
insert x Hoja = Nodo Hoja x Hoja
insert x (Nodo i c d )
    | x < c = Nodo (insert x i) c d
    | otherwise = Nodo i c (insert x d)

load :: Ord a => [a] -> ABB a
load [] = Hoja
load (x:xs) = insert x (load xs)


inOrder :: Ord a => ABB a -> [a]
inOrder Hoja = []
inOrder (Nodo i x d) = inOrder i ++ (x:inOrder d)

treeSort ::Ord a => [a] -> [a] 
treeSort [] = []
treeSort xs = inOrder (load xs)

