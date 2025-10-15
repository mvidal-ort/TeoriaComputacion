{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea2 where
import Prelude


--1) Tipos para representar programas, expresiones y valores de Imp.

type X = String -- Variable
type C = String -- Constante

data Expr = Const C [Expr]  -- Expresiones
          | Var X
          deriving (Show, Eq)

data P = Assign [X] Expr      -- Programas    
          | Local [X] P          
          | Seq P P                
          | Case X [B]         
          | While X [B]       
          deriving (Show, Eq)

type B = (C,[X],P) -- Ramas

data V = ConstV C [V]  -- Valores
           | Null                  
           deriving (Show, Eq)

--2) Definir el tipo de la memoria y operar sobre ella

type M = [(X,V)]  -- Memoria

lkup :: X-> M -> Maybe V
lkup x [] = Nothing
lkup x ((x1,v1):ys)
        | (x1 == x) = Just v1
        | otherwise = lkup x ys  

-- Actualización de la memoria
upd :: [(X, V)] -> M -> M
upd [] mem = mem
upd ((x1, v1):xs) [] = (x1, v1) : upd xs [] 
upd ((x1, v1):xs) ((x2, v2):ys) =
    case lkup x1 ((x2, v2):ys) of
        Nothing -> (x1, v1) : upd xs ((x2, v2):ys)
        Just _  -> upd xs (upd_aux (x1, v1) ((x2, v2):ys))

-- Aux actualizacion memoria
upd_aux :: (X,V) -> M -> M 
upd_aux tupla [] = [tupla]
upd_aux (x1,v1) ((x2,v2):xs)
        | (x1 == x2) = (x1,v1):xs
        | otherwise = (x2,v2):(upd_aux (x1,v1) xs)

-- Alta
create :: [X] -> M -> M
create vars mem = map (\x -> (x, Null)) vars ++ mem

-- Baja
deleteFirst :: X -> M -> M
deleteFirst _ [] = [] 
deleteFirst x ((y, v):ys)
    | x == y    = ys  
    | otherwise = (y, v) : deleteFirst x ys 

-- Baja de todas las ocurrencias
deleteAll :: X -> M -> M
deleteAll _ [] = [] 
deleteAll x ((y, v):ys)
    | x == y    = deleteAll x ys 
    | otherwise = (y, v) : deleteAll x ys 

-- 3) Funcion de evaluacion de expresiones

eval :: Expr -> M -> V
eval (Const c params) mem = ConstV c (map (`eval` mem) params)
eval (Var x) mem = case lkup x mem of
    Just v  -> v     
    Nothing -> Null 

-- 4) Funcion de ejecucion de un programa Imp

-- Función de ejecución para programas
exec :: P -> M -> M
exec (Assign vars expr) mem = foldl (\m x -> upd [(x, valor)] m) mem vars
  where valor = eval expr mem


exec (Local vars p) mem = exec p (create vars mem) 
  where
    memTemp = exec p (create vars mem)
    deleteLocals [] m = m
    deleteLocals (v:vs) m = deleteLocals vs (deleteAll v m)
    memOriginal = deleteLocals vars memTemp


exec (Seq p1 p2) mem = exec p2 (exec p1 mem)


exec (Case x bs) mem = case lkup x mem of
    Just (ConstV c []) -> execBranch c bs mem
    _ -> mem  
  where
    execBranch _ [] m = m
    execBranch c ((cond, vars, p):bs) m
      | c == cond = exec p m
      | otherwise = execBranch c bs m


exec (While x bs) mem = case lkup x mem of
    Just (ConstV c []) -> execBucle c bs mem
    _ -> mem  
  where
    execBucle c bs m = case searchBranch c bs of
        Just p -> exec (Seq p (While x bs)) m
        Nothing -> m
    searchBranch _ [] = Nothing
    searchBranch c ((cond, _, p):bs)
      | c == cond = Just p
      | otherwise = searchBranch c bs


-- 5) Funciones Imp embebidas en Haskell
---- Me doy cuenta que no entra a los while o no se esta modificando la variable como debería.

parImp :: X -> X -> P
parImp x res = 
    Local ["x_prima"] 
    (Seq (Assign ["x_prima"] (Var x))            
    (Seq (Assign [res] (Const "True" []))        
    (While "x_prima"                             
        [("S", ["y"], Seq 
            (Case res 
                [("True", [], Assign [res] (Const "False" [])), 
                 ("False", [], Assign [res] (Const "True" []))] 
            )
            (Assign ["x_prima"] (Var "y"))                      			
        )]
    )))
		      
suma :: X -> X -> X -> P 
suma m n res = 
    Local ["m_prima", "n_prima"] 
    (Seq (Assign ["m_prima"] (Var m)) 
    (Seq (Assign ["n_prima"] (Var n)) 
    (Seq (Assign [res] (Const "O" []))
    (Seq
        (While "m_prima" 
            [("S", ["x"], Seq 
                (Assign [res] (Const "S" [Var res]))  
                (Assign ["m_prima"] (Var "x"))        
            )]
        )        
        (While "n_prima" 
            [("S", ["y"], Seq 
                (Assign [res] (Const "S" [Var res]))  
                (Assign ["n_prima"] (Var "y"))        
            )]
        )
    ))))

largo :: X -> X -> P
largo xs res = 
    Local ["xs_prima", "contador"] 
    (Seq (Assign ["xs_prima"] (Var xs))            
    (Seq (Assign ["contador"] (Const "O" []))      
    (Seq 
        (While "xs_prima"                          
            [("S", ["y"], Seq 
                (Assign ["contador"] (Const "S" [Var "contador"]))  
                (Assign ["xs_prima"] (Var "y"))                     
            )]
        )
        (Assign [res] (Var "contador"))                    
    )))

igualdadN :: X -> X -> X -> P
igualdadN m n res = 
    Local ["m_prima", "n_prima"] 
    (Seq (Assign ["m_prima"] (Var m))                      
    (Seq (Assign ["n_prima"] (Var n))                      
    (Seq (Assign [res] (Const "True" []))                  
    (Seq        
        (While "m_prima" 
            [("S", ["x"], Case "n_prima" 
                [("O", [], Assign [res] (Const "False" [])),      
                 ("S", ["y"], Seq (Assign ["m_prima"] (Var "x"))  
                                  (Assign ["n_prima"] (Var "y")))] 
            )]
        )        
        (Case "n_prima" 
            [("S", ["_"], Assign [res] (Const "False" [])),         
             ("O", [], Assign [res] (Var res))]                     
        )
    ))))

concatImp :: X -> X -> X -> P
concatImp l1 l2 res = 
    Local ["l1_prima"] 
    (Seq (Assign ["l1_prima"] (Var l1))   
    (Seq (Assign [res] (Var l2))          
    (While "l1_prima"                     
        [("S", ["x"], Seq 
            (Assign [res] (Const "S" [Var res]))
            (Assign ["l1_prima"] (Var "x"))     
        )]
    )))

