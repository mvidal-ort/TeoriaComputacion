{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico1 where

-- 1. Declarar un tipo inductivo (data) apropiado para representar las expresiones
-- (sintaxis abstracta) de χ.

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

-- 2. Declarar tipos inductivos (data) apropiados para representar a los valores
-- y formas canonicas debiles de χ.
data V = 
        ConsV K [V]
    |   AbsV X E -- este es un lambda, en la clase usó LamV String Exp
    deriving (Show)

data W =
        ConsW K [E]
    |   AbsW X E -- este es un lambda, en la clase usó LamW String Exp

-- 3. Definir el tipo de las sustituciones, ası como el efecto de ellas sobre
-- expresiones χ (para esto se requiere tambien definir las operaciones de
-- busqueda y bajas).
type Sigma = [(X,E)]

-- Busqueda. Si σ es una sustitucion y x una variable, entonces σx es la expresion asociada a x en σ
-- , o la misma x si esta no se encuentra definida en σ, asımismo si la variable x se encuentra definida
-- multiples veces en σ, se devolvera la primer ocurrencia de la variable en la sustitucion.
busqueda :: X -> Sigma -> E
busqueda x [] = Var x
busqueda x ((x',e):s')
    | x == x' = e
    | otherwise = busqueda x s'

-- Bajas. Si σ es una sustitucion y x una lista de variables, entonces σ-x es la sustitucion (σ′) que
-- resulta de borrar de la tabla σ todas las entradas correspondientes a las variables en xTecho.

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

-- Sustitucion (Efecto) o Efecto de la sustitucion. El efecto de una sustitucion σ sobre una expresion e sera
-- escrito eσ y tiene la precedencia o prioridad mas alta entre todas las operaciones junto a las cuales aparece.
-- Se acepta adicionalmente la notacion del efecto de una sustitucion sobrecargada para ramas, listas de ramas
-- y lista de expresiones. 
efecto :: E -> Sigma -> E
efecto (Var x) s =  busqueda x s -- devuelve la e asociada a x si esta, sino devuelve x (que tambien es una e)
efecto (Cons k es) s = Cons k (map (`efecto` s) es) -- 
efecto (Abs x e) s = Abs x (efecto e (baja s x))
efecto (Ap e1 e2) s = Ap (efecto e1 s) (efecto e2 s)
efecto (Case e bs) s = Case (efecto e s) (map (`efectoRama` s) bs) -- Cuando aplico una sustitución a un case, la aplico al discriminante y también a cada rama, cuidando de no sustituir las variables que esa rama introduce como parámetros.
efecto (Rec x e) s = Rec x (efecto e (baja s x))

efectoRama :: B -> Sigma -> B
efectoRama (k,(xs,e)) s = (k,(xs, efecto e (bajas s xs)))

-- 4. ¿Es necesaria la sustituci´on multiple o podr´ıamos haber usado la sustitucion 
-- simple iterada multiples veces en la definicion de sustituciones?
-- Encontrar un ejemplo en χ donde ambas no coinciden para convencerse de que es necesario.

-- La clave es que ninguna sustitución afecta a las imágenes de las demás.
-- Se aplican todas “en paralelo” sobre la expresión original.
-- Aquí aplicamos las sustituciones una tras otra.
-- Ejemplo: (((e [x1 ↦ e1]) [x2 ↦ e2]) ... [xn ↦ en])
-- Esto significa que cuando reemplazamos x1 por e1, 
-- en el paso siguiente esa e1 también puede ser modificada por las sustituciones siguientes.
-- El resultado puede depender:
--   del orden en que apliques las sustituciones,
--   y de si las imágenes (e1, e2, …) contienen variables que también aparecen en la sigma.

-- Ejemplo:
-- Sustitución: σ=[x↦y,  y↦O]
-- Expresión: Var "x"
-- (a) Sustitución múltiple:  Var "x"[σ]=Var "y"
-- Sólo miramos la expresión original: x se reemplaza por y. 
-- El y ↦ O no afecta aquí porque la sustitución se aplica simultáneamente.
-- (b) Sustitución secuencial
-- Primero:Var "x"[x↦y]=Var "y"
-- Después aplicamos [y \mapsto O]: Var "y"[y↦O]=Cons "O"[]
-- Resultado: O.

 -- La sustitución múltiple garantiza que la operación no depende del orden de la sigma.
-- Siempre se entiende como “reemplazo simultáneo de todas las variables libres”.
-- La sustitución secuencial puede dar resultados distintos según el orden, porque las imágenes se van modificando.
-- Eso genera efectos de encadenamiento que no corresponden a la semántica estándar de lenguajes formales

-- 5. Definir la funcion (parcial) de evaluacion debil.
weak :: E -> W
weak (Cons k es) = ConsW k es -- (Cons k es) ya es una forma debil, evalua debilmente a lo mismo
weak (Abs x e) = AbsW x e  -- Idem anterior
weak (Ap e e') = case weak e of -- surge de dos reglas, en Ap e e'
    -- quiero evaluar e ya que tiene que evaluar a una funcion o un constructor, 
    -- que es a lo unico a lo que le puedo pasar argumentos en la aplicación, esos son los casos del case 
    AbsW x e'' -> weak (efecto e'' [(x,e')]) 
    ConsW k es -> ConsW k (es++[e'])  
weak (Case e bs) = case weak e of -- cuando evaluo debilmente el case, me tiene que dar un constructor con cosas
    ConsW k es -> case buscarRama bs k of --resulta ser un constructor k con argumentos, se busca la rama que corresponde a k.
        (xs, e') -> case length xs == length es of -- Si el número de parámetros coincide, se sustituyen los parámetros por los argumentos.
            True -> weak (efecto e' (zip xs es)) --El cuerpo de la rama (ya con sustitución hecha) es la nueva expresión a reducir.
weak (Rec x e) = weak (efecto e [(x, Rec x e)])

buscarRama :: [B] -> K -> ([X],E)
buscarRama ((k,xse):xs) k'
    | k == k' = xse
    | otherwise = buscarRama xs k'

-- en la evaluación debil, no hay una regla en la que se evalue una variable
-- No podemos trabajar con expresiones abiertas, solo cerradas. no hay variables libres, nunca deberia llegar a una variable para evaluar
-- antes de llegar a donde esta la variable, deberia haber sido sustituida por algo

-- 6. Para la evaluacion completa:
-- (a) Definir en deduccion natural el conjunto que reglas que la caracterizan.




-- (b) Definir la funcion de evaluacion completa en base a dichas reglas.
eval :: E -> V
eval e = case weak e of
    AbsW x e -> AbsV x e
    ConsW k es -> ConsV k (map eval es)


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