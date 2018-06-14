module ModuloListas where
import ModuloNListas

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.

--seleciona o menor elemento
menorElem xs
            | null (tail xs) || null xs = xs   
            | otherwise = if xs!!0 > xs!!(length xs -1)
                            then menorElem (tail xs)
                            else menorElem (init xs)

filter xs pred = [x|x<-xs,pred]

--mergesort
intercala xs ys = if (null xs) || (null ys)
    then xs ++ ys
    else if second4 (head xs) <= second4 (head ys)
    then head xs : intercala (tail xs) ys
    else head ys : intercala xs (tail ys)

mergesortFila xs = if null (tail xs)
                then xs
                else intercala (mergesortFila m) (mergesortFila n)
                where
                    m = take k xs
                    n = drop k xs
                    k = div (length xs) 2

--tempo de atendimento para a fila de navios
tempoNavios naviosAlocados infoPorto = sum [(infoPorto!!(idBercoAlocado))!!x | x<-(idsNavioAlocado naviosAlocados)]
--id do navio dentro de naviosAlocadosbercox
idsNavioAlocado naviosAlocados = [first4 ((second2 naviosAlocados)!!x) | x<-[0..length (second2 naviosAlocados) -1]]
--id do berco dentro de naviosAlocadosbercox
idBercoAlocado naviosAlocados = first3 (first2 naviosAlocados)