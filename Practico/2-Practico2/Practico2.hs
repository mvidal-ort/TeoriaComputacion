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
eval (K c es) m = Kv c (map (`eval` m) es ) -- este un truco para para que  m sea lo segundo que recibe eval
eval (Var x) m = case lookupM x m of        -- se convierte en notacion infija
                Just v -> v
                Nothing -> error "no existe "
         
exec :: Program -> M -> M

exec (Asig pairs) m =  --(asignacion multiple), pairs es la lista de id-Expresion
  let (xs, es) = unzip pairs --separa la expresion del identificador
      vs = map (`eval` m) es --evalua las expresiones bajo y las asigna a una lista de valores resultantes
  in updateM (zip xs vs) m -- hace el update de la memoria, asignanco los valores obtenidos a los ids anteriores


exec (Sec p1 p2) m =  
  let m' = exec p1 m -- ejecuta p1 sobre m y devuelve m'
  in exec p2 m' -- ejecuta p2 sobre la m' que devolvio antes

exec (Local xs p) m =
  let m'  = altaM xs m
      m'' = exec p m'
  in bajaM xs m''

exec (Case x bs) m = case (eval (Var x) m) of
  Kv c vs -> case (lookup c bs) of
    Just (xs, p) -> case (length xs == length vs) of
      True -> exec (Local xs (Asig (zip xs (map valorAExpresion vs)) `Sec` p)) m


exec (While x bs) m =
  case eval (Var x) m of
    Kv c vTecho ->
      case buscarEnRamas c bs of
        Just (xTecho, p)
          | length xTecho == length vTecho ->
              let m' = exec (Local xTecho (Sec (Asig (zip xTecho (map valorAExpresion vTecho))) p)) m
              in exec (While x bs) m'  -- while-ii
        _ -> m  -- si no hay rama o aridad distinta -> termina
    _ -> m      -- si eval no da Kv (por ejemplo NULL), también termina

------------------------------------------------------------
-- Auxiliar para Case y While: busca la rama por constructor
------------------------------------------------------------

buscarEnRamas :: Id -> [B] -> Maybe ([Id], Program)
buscarEnRamas id bs = lookup id bs

valorAExpresion :: V -> E
valorAExpresion (Kv id vs) = K id (map valorAExpresion vs)

-------------------
-------------------

par :: Program
par =
  Asig [("res", K "False" [])] `Sec`
  While "n"
    [ ("Zero", ([], Asig [("res", K "True" [])]))
    , ("Succ", (["x"],
        Case "x"
          [ ("Zero", ([], Asig [("res", K "False" [])]))
          , ("Succ", (["y"], Asig [("n", Var "y")]))
          ]
      ))
    ]

-- 0
n0 = Kv "Zero" []

-- 1 = Succ Zero
n1 = Kv "Succ" [n0]

-- 2 = Succ (Succ Zero)
n2 = Kv "Succ" [n1]

-- 3 = Succ (Succ (Succ Zero))
n3 = Kv "Succ" [n2]

-- 4 = Succ (Succ (Succ (Succ Zero)))
n4 = Kv "Succ" [n3]

--m0 n = [("n", n)]
m0 n = [("n", n), ("res", NULL)]


