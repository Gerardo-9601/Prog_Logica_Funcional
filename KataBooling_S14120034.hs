-- NOTA
-- PROFE SI FUNCIONA PERO EN DOS PASOS NO SUPE COMO HACERLE PARA QUE FUNCIONARA SOLO EN UN PASO
-- PRIMERO DEBEMOS DE EJECUTAR LA FUNCION  toframes[1,4,4,5,6,4,5,5,10,0,1,7,3,6,4,10,2,8,6]
-- NOS REGRESARA Just[Open 1 4,Open 4 5,Spare 6 5,Spare 5 10,Strike 0 1 Open 0 1,Spare 7 6,Spare 6 10,Strike 2 8
--,Spare 2 6]
-- DESPUES LLAMAMOS A LA FUNCION SCORE CON LOS DATOS DEVUELTOS OMITIENTO EL JUST EJEMPLO
-- score[Open 1 4,Open 4 5,Spare 6 5,Spare 5 10,Strike 0 1 Open 0 1,Spare 7 6,Spare 6 10,Strike 2 8,Spare 2 6]
-- Y NOS REGRESA EL PUNTAJE TOTAL OBTENIDO QUE EN ESTE CASO ES 133


-- PERO SI QUIERO LLAMAR TODA LA FUNCION JUNTA NO FUNCIONA POR QUE toframe ME RETORNA LOS FRAMES ENVUELTOS
-- SI HAGO ESTO NO FUNCIONA score[toframes[1,4,4,5,6,4,5,5,10,0,1,7,3,6,4,10,2,8,6]]
-- ES COMO SI LE MANDARA ESTO score[Just[Open 1 4,Open 4 5,Spare 6 5,Spare 5 10,Strike 0 1 Open 0 1,Spare 7 6,Spare 6 10,Strike 2 8,Spare 2 6]]

-- int arr=[1,4,4,5,6,4,5,5,10,0,1,7,3,6,4,10,2,8,6]

data Frame = Open Int Int
  |Spare Int Int
  |Strike Int Int 
  deriving (Eq, Show)

toframes::[Int]-> Maybe [Frame]
toframes pins = go 1 pins
  where 
    go 10 [x,y]
     |x + y < 10 = Just[Open x y]
     |otherwise = Nothing
    go 10 [x,y,z]
     |x ==10 = Just [Strike y z]
     |x + y ==10 = Just [Spare x z]
     |otherwise = Nothing
    go n (x:y:z:ys)
     | x == 10 = fmap (Strike y z :) $ go (n+1)(y:z:ys)
     | x + y == 10 = fmap (Spare x z :) $ go (n+1)(z:ys)
     | x + y < 10 = fmap (Open x y :) $ go (n+1)(z:ys)
     |otherwise = Nothing
    go _ _ = Nothing


frameScore :: Frame -> Int
frameScore (Open x y) =  x + y 
frameScore (Spare _ y) =   10 + y 
frameScore (Strike x y) =  10 + x + y


score :: [Frame] -> Int 
score =sum . fmap frameScore


