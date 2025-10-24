{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico2 where

import Prelude

data Program = 
    Asig [(Id, E)] 
    | Local [Id] Program 
    | Sec Program Program 
    | Case Id [B] 
    | While Id [B] 
    deriving (Show) 

data E = 
    K Id [E] 
    | Var Id 
    deriving (Show) 

data V = 
    Kv Id [V] 
    | NULL 
    deriving (Show) 

type Id = String 
type B = (Id, ([Id], Program)) 
type M = [(Id, V)]

lookupM :: Id -> M -> Maybe V
lookupM _ [] = Nothing
lookupM x' ((x,v):xs)
    | x' == x = Just v
    | otherwise = lookupM x xs

updateM :: [(Id,V)] -> M -> M
updateM [] m = m
updateM ((x,v): xs) m = updateM xs (updateOne x v m)

updateOne :: Id -> V -> M -> M
updateOne x' v' [] = [(x',v')] 
updateOne x' v' ((x,v):xs) 
    | x' == x = (x', v'):xs
    | otherwise = updateOne x' v' xs

altaM :: [Id] -> M -> M
altaM xs m = map (\x -> (x,NULL)) xs ++ m

bajaM :: [Id] -> M -> M
bajaM [] m = m
bajaM (x:xs) m = bajaM xs (bajaOne x m)

bajaOne :: Id -> M -> M
bajaOne _ [] = []
bajaOne x' ((x,v):xs)
    | x' == x = xs
    | otherwise = (x,v):bajaOne x' xs

eval :: E -> M -> V
eval (K c es) m = Kv c (map (`eval` m) es )
eval (Var x) m = case lookupM x m of 
                Just v -> v
                Nothing -> error "no existe "
 
exec :: Program -> M -> M

-- 1. Asignación múltiple
exec (Asig pairs) m =  --(asignacion multiple), pairs es la lista de id-Expresion
  let (xs, es) = unzip pairs --separa la expresion del identificador
      vs = map (`eval` m) es --evalua las expresiones bajo y las asigna a una lista de valores resultantes
  in updateM (zip xs vs) m -- hace el update de la memoria, asignanco los valores obtenidos a los ids anteriores

-- 2. Secuencia
exec (Sec p1 p2) m =  
  let m' = exec p1 m -- ejecuta p1 sobre m y devuelve m'
  in exec p2 m' -- ejecuta p2 sobre la m' que devolvio antes

-- 3. Local
exec (Local xs p) m =
  let m'  = altaM xs m
      m'' = exec p m'
  in bajaM xs m''

-- 4. Case
-- exec (Case x branches) m =
--   case lookupM x m of
--     Just (Kv c vs) ->
--       case lookupBranch c branches of
--         Just (params, body)
--           | length params == length vs ->
--               let m'  = altaM params m
--                   m'' = updateM (zip params vs) m'
--               in bajaM params (exec body m'')
--         _ -> m  -- no hay coincidencia o aridad incorrecta
--     _ -> m      -- variable no definida o NULL


exec (Case x bs) m = case (eval (Var x) m) of
  Kv c vs -> case (lookup c bs) of
    Just (xs, p) -> case (length xs == length vs) of
      True -> exec (Local xs (Asig (zip xs (map valorAExpresion vs)) `Sec` p)) m


-- 5. While
exec (While x branches) m =
  case lookupM x m of
    Just (Kv c vs) ->
      case lookupBranch c branches of
        Just (params, body)
          | length params == length vs ->
              let m'  = altaM params m
                  m'' = updateM (zip params vs) m'
                  m''' = exec body m''
              in exec (While x branches) (bajaM params m''')
        _ -> m
    _ -> m

-- exec m (While x bs) = case (evalE m (Var x)) of
--   Kv c vs -> case (lookup c bs) of -- hay una rama correspondiente
--     Just (xs, p) -> case (length xs == length vs) of
--       True ->                                                            
--         let assign = Asig (zip xs (map valorAExpresion vs))          -- construimos local xs { xs := vs; p } ; while x is bs
--             localBlock = Local xs (Sec assign p)
--             fullProg = Sec localBlock (While x bs)
--         in exec m fullProg
--       False -> m -- aridad incorrecta → no hace nada
--     Nothing -> m -- no hay rama → termina el while
--   _ -> m -- x no tiene un valor constructor válido

-- exec m (While x bs) =
--   let Kv c vTecho = evalE m (Var x)
--    in case buscarEnRamas c bs of
--         Just (xTecho, p) -> case (length xTecho) == (length vTecho) of
--           True ->
--             let m' = exec m (Local xTecho (Sec (Asig (zip xTecho (map valorAExpresion vTecho))) p))
--              in exec m' (While x bs) -- while-ii
--         Nothing -> m -- while-i

------------------------------------------------------------
-- Auxiliar para Case y While: busca la rama por constructor
------------------------------------------------------------

lookupBranch :: Id -> [B] -> Maybe ([Id], Program)
lookupBranch _ [] = Nothing
lookupBranch c ((c',bp):bs)
  | c == c'   = Just bp
  | otherwise = lookupBranch c bs

-- buscarEnRamas :: Id -> [B] -> Maybe ([Id], Program)
-- buscarEnRamas id bs = lookup id bs

valorAExpresion :: V -> E
valorAExpresion (Kv id vs) = K id (map valorAExpresion vs)