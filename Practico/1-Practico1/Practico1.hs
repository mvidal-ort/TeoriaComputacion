{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico1 where

-- en la evaluación debil, no hay una regla en la que se evalue una variable
-- No podemos trabajar con expresiones abiertas, solo cerradas. no hay variables libres, nunca deberia llegar a una variable para evaluar
-- antes de llegar a donde esta la variable, deberia haber sido sustituida por algo
-- weak :: E -> W
-- weak (Cons k es) = ConsW k es -- (Cons k es) ya es una forma debil, evalua debilmente a lo mismo
-- weak (Abs x e) = AbsW x e -- Idem anterior
-- Lo siguiente, eurge de dos reglas, en Ap e1 e2,  quiero evaluar e1 ya que tiene que evaluar a una funcion o un constructor, 
-- que es a lo unico a lo que le puedo pasar argumentos en la aplicación, esos son los casos del case (evaluando debilmente e1)
-- weak (Ap e e') = case weak e of 
--     AbsW x e'' -> weak (efecto e'' [(x,e')])
--     ConsW k es -> ConsW k (es++[e']) -- Un constructor al cual le aplico cosas, es el constructor con la lista de esas cosas
-- weak (Case e bs) = case weak e of -- cuando evaluo debilmente el case, me tiene que dar un contructor con cosas, es el unico valor weak que me puede dar
--     ConsW k es -> case buscarRama bs k of
--         (xs, e') -> case length xs == length es of
--             True -> weak (efecto e' (zip xs es))
-- weak (Rec x e) = weak (efecto e [(x, Rec x e)])

-- Ejercicio 7, chi es no tipado, pero cuando se embebe en Haskell, necesitamos tipos.
-- El tipo que vamos a usar es el data de E, es decir. Por lo tanto las funciones de este ejercicio van a tener ese tipo
-- En la solucion mia, puse de E -> E, esto esta mal, no hay que hacer esto
-- Estas funciones son programas que existen por si solas
-- en la solución de los profesores es de tipo E solo,


-- or' :: E
-- or' = Abs "b1" (Abs "b2" (Case (Var "b1") [
--         ("True",([], Cons "True" [])),
--         ("False",([], Var "b2"))
--     ]))

-- triple :: E
-- triple = Rec "triple" (Abs "n" (Case (Var "n") [
--         ("O", ([], Cons "O" [])),
--         ("S",(["x"], Cons "S" [Cons "S" [Cons "S" [Ap (Var "triple") (Var "x")]]]))
--     ]))

-- duplicar :: E
-- duplicar = Rec "duplicar" (Abs "l" (Case (Var "l") [
--         ("[]",([],Cons "[]" [])),
--         (":",(["x","xs"], Cons ":" [Var "x", Cons ":" [Var "x", Ap (Var "duplicar") (Var "xs")]]))
--     ]))

-- ramaC :: E
-- ramaC = Rec "ramaC" (Abs "t" (Case (Var "t") [
--         ("H", (["x"], Cons ":" [Var "x", Cons "[]" []])),
--         ("N",(["i","c","d","x"], Cons ":" [Var "x", Ap (Var "ramaC") (Var "c")]))
--     ]))