{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Dada una fórmula booleana en forma normal conjuntiva (CNF), decidir si existe una asignación de valores
-- de verdad a sus variables que haga que la fórmula sea verdadera.
-- La Forma Normal Conjuntiva (FNC) conjunción de disyunciones, donde se unen varias cláusulas mediante ANDs
-- y cada cláusula contiene literales unidos por ORs

module LambdaInterpreter where

import Data.List (nub, delete)

-- Ejercicio 1: Representación de términos
-- M :: = x ∣ λx.M ∣ M N

data Expr
    = Var String
    | Lam String Expr
    | App Expr Expr
    deriving (Eq, Show)

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
-- mediante alpha-renaming, pero no maneja correctamente casos de
-- lambdas anidados que reutilizan el mismo nombre de variable
-- (shadowing), por ejemplo: Lam "x" (Lam "x" body).
-- En esos casos el renombrado puede propagarse más allá del scope deseado.


