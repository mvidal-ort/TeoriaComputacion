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
