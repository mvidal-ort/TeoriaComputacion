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
 


