{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico1 where

-- en la evaluación debil, no hay una regla en la que se evalue una variable
-- No podemos trabajar con expresiones abiertas, solo cerradas. no hay variables libres, nunca deberia llegar a una variable para evaluar
-- weak :: E -> W
-- weak (Cons k es) = ConsW k es
-- weak (Abs x e) = AbsW x e
-- weak (Ap e e') = case weak e of
--     AbsW x e'' -> weak (efecto e'' [(x,e')])
--     ConsW k es -> ConsW k (es++[e'])
-- weak (Case e bs) = case weak e of
--     ConsW k es -> case buscarRama bs k of
--         (xs, e') -> case length xs == length es of
--             True -> weak (efecto e' (zip xs es))
-- weak (Rec x e) = weak (efecto e [(x, Rec x e)])