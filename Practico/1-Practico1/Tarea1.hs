module Tarea1 where

------------
--SOLUCIÓN--
------------

--Ejercicio 1
data E =
        Var X
    |   Cons K [E]
    |   Abs X E
    |   Ap E E
    |   Case E [B]
    |   Rec X E
    deriving (Show)
type B = (K,([X],E))
type X = String
type K = String

--Ejercicio 2
data V = 
        ConsV K [V]
    |   AbsV X E
    deriving (Show)

data W =
        ConsW K [E]
    |   AbsW X E

--Ejercicio 3

type Sigma = [(X,E)]

busqueda :: X -> Sigma -> E
busqueda x [] = Var x
busqueda x ((x',e):s')
    | x == x' = e
    | otherwise = busqueda x s'

bajas :: Sigma -> [X] -> Sigma
bajas s [] = s
bajas s (x:xs) = bajas (baja s x) xs

baja :: Sigma -> X -> Sigma
baja [] x = []
baja (xe@(x,e):s') x'
    | x == x' = baja s' x'
    | otherwise = xe:(baja s' x')

-- Alternativa sin usar el as-pattern (@)
-- baja [] key = [] 
-- baja ((k,v):rest) key
--   | k == key  = baja rest key
--   | otherwise = (k,v) : baja rest key


efecto :: E -> Sigma -> E
efecto (Var x) s =  busqueda x s
efecto (Cons k es) s = Cons k (map (`efecto` s) es)
efecto (Abs x e) s = Abs x (efecto e (baja s x))
efecto (Ap e1 e2) s = Ap (efecto e1 s) (efecto e2 s)
efecto (Case e bs) s = Case (efecto e s) (map (`efectoRama` s) bs)
efecto (Rec x e) s = Rec x (efecto e (baja s x))

efectoRama :: B -> Sigma -> B
efectoRama (k,(xs,e)) s = (k,(xs, efecto e (bajas s xs)))

weak :: E -> W
weak (Cons k es) = ConsW k es
weak (Abs x e) = AbsW x e
weak (Ap e e') = case weak e of
    AbsW x e'' -> weak (efecto e'' [(x,e')])
    ConsW k es -> ConsW k (es++[e'])
weak (Case e bs) = case weak e of
    ConsW k es -> case buscarRama bs k of
        (xs, e') -> case length xs == length es of
            True -> weak (efecto e' (zip xs es))
weak (Rec x e) = weak (efecto e [(x, Rec x e)])

buscarRama :: [B] -> K -> ([X],E)
buscarRama ((k,xse):xs) k'
    | k == k' = xse
    | otherwise = buscarRama xs k'

eval :: E -> V
eval e = case weak e of
    AbsW x e -> AbsV x e
    ConsW k es -> ConsV k (map eval es)

--Ejercicio 6
or' :: E
or' = Abs "b1" (Abs "b2" (Case (Var "b1") [
        ("True",([], Cons "True" [])),
        ("False",([], Var "b2"))
    ]))

triple :: E
triple = Rec "triple" (Abs "n" (Case (Var "n") [
        ("O", ([], Cons "O" [])),
        ("S",(["x"], Cons "S" [Cons "S" [Cons "S" [Ap (Var "triple") (Var "x")]]]))
    ]))

duplicar :: E
duplicar = Rec "duplicar" (Abs "l" (Case (Var "l") [
        ("[]",([],Cons "[]" [])),
        (":",(["x","xs"], Cons ":" [Var "x", Cons ":" [Var "x", Ap (Var "duplicar") (Var "xs")]]))
    ]))

ramaC :: E
ramaC = Rec "ramaC" (Abs "t" (Case (Var "t") [
        ("H", (["x"], Cons ":" [Var "x", Cons "[]" []])),
        ("N",(["i","c","d","x"], Cons ":" [Var "x", Ap (Var "ramaC") (Var "c")]))
    ]))

-------
--FIN--
-------

-- Ejemplos para probar

-- Ejemplos para probar en consola

ej1 = Cons "True" []
ej2 = Cons "S" [Cons "S" [Cons "O" []]]
ej3 = Ap (Abs "x" (Var "x")) (Cons "True" [])
ej4 = Case (Cons "True" [])
        [ ("True",  ([], Cons "O" []))
        , ("False", ([], Cons "S" [Cons "O" []]))
        ]
ej5 = Ap (Ap or' (Cons "True" [])) (Cons "False" [])
ej6 = Ap (Ap or' (Cons "False" [])) (Cons "False" [])
ej7 = Ap triple (Cons "O" [])
ej8 = Ap triple (Cons "S" [Cons "O" []])
ej9 = Ap triple (Cons "S" [Cons "S" [Cons "O" []]])
ej10 = Ap duplicar (Cons "[]" [])
ej11 = Ap duplicar (Cons ":" [Cons "True" [], Cons ":" [Cons "False" [], Cons "[]" []]])
ej12 = Cons "H" [Cons "O" []]
ej13 = Ap ramaC ej12
ej14 = Cons "N" [Cons "O" [], ej12, Cons "H" [Cons "S" [Cons "O" []]], Cons "True" []]
ej15 = Ap ramaC ej14
