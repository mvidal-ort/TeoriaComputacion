{-# OPTIONS_GHC -fno-warn-tabs #-}

import Prelude
import System.Win32 (COORD(xPos))



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

mapP :: (a -> b) -> [a] -> [b]
mapP _ [] = []
mapP f (x:xs) = f x : (mapP f xs)

-- (c) filter :: (a -> Bool) -> [a] -> [a]
-- Ej. filter even [1,6,5,3,2] = [6,2]

filterP :: (a -> Bool) -> [a] -> [a]
filterP _ [] = []
filterP f (x:xs) 
    | f x == True = x : filterP f xs
    | f x == False = filterP f (xs)

-- (d) zip :: [a] -> [b] -> [(a,b)]
-- Ej. zip [1,5,4,2] [True, True, False] = [(1,True), (5,True), (4,False)]

zipP :: [a] -> [b] -> [(a,b)]
zipP [] [] = []
zipP (x:xs) [] = []
zipP [] (y:ys)  = []
zipP (x:xs) (y:ys)  = (x,y):zipP (xs)(ys)

-- (4) Listas con tipos:

-- (a) sum :: [Integer] -> Integer
-- Ej. sum [6,5,2] = 6 + 5 + 2 = 13
sumP :: [Integer] -> Integer
sumP [] = 0
sumP (x:xs) = x + sumP xs

-- (b) prod :: [Integer] -> Integer
-- Ej. prod [1,5,2,0] = 1 * 5 * 2 * 0 = 0
prodP :: [Integer] -> Integer
prodP [] = 1
prodP (x:xs) = x * prodP xs

-- (c) and :: [Bool] -> Bool
-- Ej. and [True, True, False] = True && True && False = False
andPL :: [Bool] -> Bool
andPL [] = True
andPL (x:xs) 
    | x == True = andPL xs
    | x == False = False

-- (d) or :: [Bool] -> Bool
-- Ej. or [True, True, True] = True || True || True = True
orPL :: [Bool] -> Bool
orPL [] = False
orPL (x:xs) 
    | x == False = orPL xs
    | x == True = True

-- Arboles

data Arb a = H a | Nd (Arb a) a (Arb a) deriving Show

--(a) λ − notation y case:
cantNodos:: Arb a -> Integer
cantNodos = \a -> case a of {
        H x -> 0;
        Nd i x d -> 1+(cantNodos i)+(cantNodos d)
    }
--Pattern-Matching:
cantNodosP:: Arb a -> Integer
cantNodosP (H x) = 0
cantNodosP (Nd i x d) = 1+(cantNodosP i)+(cantNodosP d)


-- (b) cantHojas :: Arb a -> Integer
-- (c) cantA :: Arb a -> Integer
-- (d) listA :: Arb a -> [a]
-- (e) mapF :: (a -> b) -> Arb a -> Arb b



data ABB a = Hoja | Nodo (ABB a) a (ABB a) deriving Show

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

