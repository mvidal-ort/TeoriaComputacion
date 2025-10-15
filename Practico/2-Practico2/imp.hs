{-# OPTIONS_GHC -fno-warn-tabs #-}

module Imp where

type Id = String

data Program
  = Asig [(Id, E)]
  | Local [Id] Program
  | Sec Program Program
  | Case Id [B]
  | While Id [B]
  deriving (Show)

type B = (Id, ([Id], Program))

data E
  = K Id [E]
  | Var Id
  deriving (Show)

-- semantica

data V
  = Kv Id [V]
  | NULL
  deriving (Show)

type M = [(Id, V)]

altaMultiple :: M -> [Id] -> M
altaMultiple m [] = m
altaMultiple m (id : idvs) = altaMultiple ((id, NULL) : m) idvs

updateMultiple :: M -> [(Id, V)] -> M
updateMultiple m [] = m
updateMultiple m ((id, v) : idvs) = updateMultiple ((id, v) : m) idvs

deleteSingle :: M -> Id -> M
deleteSingle [] id = error "no definido"
deleteSingle m [] = m
deleteSingle ((id, v) : idvs) aBorrar
  | id == aBorrar = idvs
  | otherwise = (id, v) : (deleteSingle idvs aBorrar)

deleteMultiple :: M -> [Id] -> M
deleteMultiple m [] = m
deleteMultiple m (id : ids) = deleteMultiple (deleteSingle m id) ids

evalE :: M -> E -> V
evalE m (K id eTecho) =
  let vTecho = map (evalE m) eTecho
   in Kv id vTecho
evalE m (Var id) = case lookup id m of
  Just x -> x

buscarEnRamas :: Id -> [B] -> Maybe ([Id], Program)
buscarEnRamas id bs = lookup id bs

valorAExpresion :: V -> E
valorAExpresion (Kv id vs) = K id (map valorAExpresion vs)

exec :: M -> Program -> M
exec m (Asig parejas) =
  let (xTecho, eTecho) = unzip parejas
   in let vTecho = map (evalE m) eTecho
       in updateMultiple m (zip xTecho vTecho)
exec m (Local xTecho p) =
  let memoriaMod = altaMultiple m xTecho
   in let m' = exec memoriaMod p
       in deleteMultiple m' xTecho
exec m (Sec p1 p2) =
  let m' = exec m p1
   in exec m' p2
exec m (Case x bs) = case (evalE m (Var x)) of
  Kv c vs -> case (lookup c bs) of
    Just (xs, p) -> case (length xs == length vs) of
      True -> exec m (Local xs (Asig (zip xs (map valorAExpresion vs)) `Sec` p))
exec m (While x bs) =
  let Kv c vTecho = evalE m (Var x)
   in case buscarEnRamas c bs of
        Just (xTecho, p) -> case (length xTecho) == (length vTecho) of
          True ->
            let m' = exec m (Local xTecho (Sec (Asig (zip xTecho (map valorAExpresion vTecho))) p))
             in exec m' (While x bs) -- while-ii
        Nothing -> m -- while-i