{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Dada una fórmula booleana en forma normal conjuntiva (CNF), decidir si existe una asignación de valores
-- de verdad a sus variables que haga que la fórmula sea verdadera.
-- La Forma Normal Conjuntiva (FNC) conjunción de disyunciones, donde se unen varias cláusulas mediante ANDs
-- y cada cláusula contiene literales unidos por ORs

module LambdaInterpreter where

import Data.List (nub, delete) 
import System.CPUTime -- Agregado para la parte2.Ej 9, comparación experimental

-- Ejercicio 1: Representación de términos
-- M :: = x ∣ λx.M ∣ M N

data Expr
    = Var String
    | Lam String Expr
    | App Expr Expr
    deriving (Eq)

-- Agregado para el ejercicio 6 - pretty printer
instance Show Expr where
    show = pretty

-- Ejercicio 2: Variables libres

fv :: Expr -> [String]
fv expr =
    case expr of
        Var x -> [x] -- siempre la var
        Lam x cuerpo -> delete x (fv cuerpo) -- toda aparicion libre de x dentro del cuerpo se elimina porque deja de ser libre
        App e1 e2 -> nub (fv e1 ++ fv e2) -- es la union de variables libres de las expresiones

-- Ejercicio 3: Sustitución captura-evitante

-- subst x n e
--sustituir todas las ocurrencias libres de x dentro de e por n
--subst :: String -> Expr -> Expr -> Expr

-- Esta auxiliar es necesaria para aplica la regla alfa, crearndo un nombre de variable nuevo
-- inventa un nombre que no esté en la lista, concatenandole " ' " a la variable hasta que no exista el nombre en la lista
freshVar :: String -> [String] -> String
freshVar x used =
    if x `elem` used
        then freshVar (x ++ "'") used
        else x

subst :: String -> Expr -> Expr -> Expr
subst x n expr =
    case expr of
        Var y -> if y == x
                 then n
                 else Var y
        App e1 e2 -> App (subst x n e1) (subst x n e2)
        Lam y cuerpo            
            | y == x -> Lam y cuerpo -- x queda ligada por este lambda            
            | y `notElem` fv n -> Lam y (subst x n cuerpo) -- no hay riesgo de captura            
            | otherwise -> -- puede haber captura
                let used = fv cuerpo ++ fv n ++ [x, y]
                    fresh = freshVar y used
                    renombrarC = subst y (Var fresh) cuerpo
                in Lam fresh (subst x n renombrarC)

-- Limitación:
-- Esta implementación de subst realiza sustitución captura-evitante
-- mediante regla alfa, pero no maneja correctamente casos de
-- lambdas anidados que reutilizan el mismo nombre de variable
-- (shadowing), por ejemplo: Lam "x" (Lam "x" cuerpo).
-- En esos casos el renombrado puede propagarse más allá del scope deseado.
-- Este caso falla:
-- :{
-- subst "x" (Var "y")
--     (Lam "y"
--         (Lam "y"
--             (App (Var "x") (Var "y"))))
-- :}
-- Deberia devolver: 

-- -- Ejercicio 4: Semántica operacional
--     step :: Expr -> Maybe Expr  -- la función debe realizar exactamente un paso de evaluación.
-- Si la expresión puede reducirse: Just exprReducida
-- Si ya no puede reducirse: Nothing

-- Estrategia de evaluación: Call-by-value
-- El orden de evaluación es:
-- 1. evaluar primero la función
-- 2. luego el argumento
-- 3. aplicar beta-reducción solo cuando el argumento sea un valor

-- Definición de valor: En este intérprete, un valor es una abstracción lambda:  λx.M
-- Variables y aplicaciones no son valores.

-- Reglas de evaluación
-- 1. Beta-reducción:   (λx.M)V → M[x := V] -- Solo cuando V sea valor.

-- 2. Aplicación izquierda-- Si:  M1 → M1' Entonces: M1 M2 → M1' M2
-- Primero se intenta reducir la función.

-- 3. Aplicación derecha-- Si:  M2 → M2' Entonces:  V M2 → V M2'
-- Solo si V ya es un valor.


-- Estrategia de implementación por casos:
-- 1. Variable:   No reduce → Nothing
-- 2. Lambda:   No se reduce el cuerpo en call-by-value → Nothing
-- 3. Aplicación:
--    Subcasos:
--    - si hay beta-reducción posible, aplicarla
--    - si no, intentar reducir izquierda
--    - si izquierda ya es valor, reducir derecha

isValue :: Expr -> Bool
isValue expr =
    case expr of
        Lam _ _ -> True
        _       -> False

step :: Expr -> Maybe Expr
step expr =
    case expr of        
        Var _ -> Nothing -- Variables no reducen        
        Lam _ _ -> Nothing -- Lambdas no reducen (call-by-value)        
        App e1 e2 ->
            -- Beta-reducción            
            if isValue e1 && isValue e2 then
                case e1 of
                    Lam x body -> Just (subst x e2 body)
                    _ -> Nothing
            -- Aplicación izquierda: primero reducir la función
            else
                case step e1 of
                    Just e1' -> Just (App e1' e2)
                    Nothing ->
                        -- Aplicación derecha: solo si izquierda ya es valor
                        if isValue e1 then
                            case step e2 of
                                Just e2' -> Just (App e1 e2')
                                Nothing -> Nothing
                        else
                            Nothing

-- Ejercicio 5: Evaluación completa
-- eval :: Expr -> Expr -- la función eval aplica repetidamente la semántica small-step hasta alcanzar una expresión que ya no pueda reducirse.

-- Estrategia:
-- 1. Aplicar step sobre la expresión actual.
-- 2. Si step devuelve Nothing: la expresión está en forma normal y se devuelve.
-- 3. Si step devuelve Just expr': continuar evaluando recursivamente expr'.

eval :: Expr -> Expr
eval expr =
    case step expr of
        Nothing -> expr
        Just expr' -> eval expr'

-- Ejercicio 6: Pretty printer

-- auxiliar para poner parentesis 
wrap :: Bool -> String -> String
wrap cond s = if cond then "(" ++ s ++ ")" else s

-- auxiliar para determinar si necesita parentesis
needsParens :: Expr -> Bool
needsParens expr =
    case expr of
        Var _ -> False
        _     -> True

pretty :: Expr -> String
pretty expr =
    case expr of
        Var x -> x
        Lam x body -> "\\" ++ x ++ "." ++ pretty body
        App e1 e2 -> wrap (needsParens e1) (pretty e1) ++ " " ++ wrap (needsParens e2) (pretty e2)
-- no renderiza bien el lambda, intento usar la retrobarra pero muestra las dos y con una sola no compila.
-- funciona con putStrLn delante pero no lo agregar a la funcion, habría que hacer un main para que 
-- maneje la interacción con la consola
-- Limitación: no me doy cuenta si está agregando paréntesis innecesarios.

-- Ejercicio 7: Sharing
-- Ejemplo: (\x. x x) E, al hacer beta reducción x := E queda: E E
-- para no tener que hacer dos evaluaciones de E, se implementa sharing

-- Entorno para guardar las evaluaciones de expresiones 
type Env = [(String, Expr)]

-- Busqueda en el entorno
lookupEnv :: String -> Env -> Maybe Expr
lookupEnv x env =
    lookup x env

-- Evaluador con sharing
evalShared :: Env -> Expr -> Expr
evalShared env expr =
    case expr of        
        Var x ->
            case lookupEnv x env of
                Just value -> evalShared env value
                Nothing -> Var x        
        Lam x body -> Lam x body        
        App e1 e2 ->
            case evalShared env e1 of
                Lam x body -> evalShared ((x, e2) : env) body
                e1' -> App e1' e2

-- Ejercicio 8: Numerales de Church

church0 :: Expr
church0 = Lam "f" (Lam "x" (Var "x"))

church1 :: Expr
church1 = Lam "f" (Lam "x" (App (Var "f") (Var "x")))

church2 :: Expr
church2 = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))

succChurch :: Expr
succChurch =
    Lam "n"
        (Lam "f"
            (Lam "x"
                (App
                    (Var "f")
                    (App
                        (App
                            (Var "n")
                            (Var "f"))
                        (Var "x")))))

plusChurch :: Expr
plusChurch =
    Lam "m"
        (Lam "n"
            (Lam "f"
                (Lam "x"
                    (App
                        (App
                            (Var "m")
                            (Var "f"))
                        (App
                            (App
                                (Var "n")
                                (Var "f"))
                            (Var "x"))))))

multChurch :: Expr
multChurch =
    Lam "m"
        (Lam "n"
            (Lam "f"
                (App
                    (Var "m")
                    (App
                        (Var "n")
                        (Var "f")))))


-- Ejercicio 9: Comparación experimental
-- El objetivo de este experimento es comparar el desempeño entre:
-- 1. evaluación tradicional basada en sustitución (sin sharing)
-- 2. evaluación con sharing explícito

-- Funcion de benchmarking
benchmark :: Expr -> IO ()
benchmark expr = do
    start1 <- getCPUTime
    let result1 = eval expr
    result1 `seq` return ()
    end1 <- getCPUTime

    start2 <- getCPUTime
    let result2 = evalShared [] expr
    result2 `seq` return ()
    end2 <- getCPUTime
    
    putStrLn ("Sin sharing: " ++ show (end1 - start1))
    putStrLn ("Con sharing: " ++ show (end2 - start2))
   

-- Expresion de prueba
-- (\x. x x x)
dupExpr :: Expr
dupExpr = App (Lam "x" (App (App (Var "x") (Var "x")) (Var "x"))) church2

-- no diferencias medibles con la implementacion de los eval con o sin sharing
