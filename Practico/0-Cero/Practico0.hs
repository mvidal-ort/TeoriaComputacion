module Practico0 where

-- eval  m (Pert z e) = let (m', C c) = eval m e    
--                         in (m', belongs z c)

-- Con Let le estoy diciendo que el resultado de eval m e, lo tire en el par (m', C c)
-- A su vez, con C c, le estoy diciendo que ese resultado es de tipo Conjunto.
-- let crea el ambiente el in permite invocar las variables en ese ambiente
-- Eventualmente se podria no poner el constructor y dejar solo (m',c) y hacer un case
-- para que la lista de enteros sea un conjunto. Es mas facil asi.
-- El problema es que asi no compila, porque el belons z c tiene que devolver un valor de tipo Bool
-- por eso hay que ponerle el constructor:
-- eval  m (Pert z e) = let (m', C c) = eval m e    
--                         in (m', B (belongs z c))


-- La recomendación es hacer un let por cada condicion que aparezca en la regla que se esta evaluando
-- Asi te podes guardar todos los nombres y usarlos en los in.

-- eval m (Union e1 e2) = let (m', C c1) = eval m e1
--                         in let (m'', C c2) = eval m' e2
--                           in (m'', C (union c1 c2))






