module ModuloListas where
import ModuloNListas

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.

--seleciona o menor elemento
menorElem xs
            | null (tail xs) || null xs = xs   
            | otherwise = if xs!!0 > xs!!(tamInd xs)
                            then menorElem (tail xs)
                            else menorElem (init xs)

maiorElem xs
            | null (tail xs) || null xs = xs   
            | otherwise = if xs!!0 < xs!!(tamInd xs )
                            then menorElem (tail xs)
                            else menorElem (init xs)                            

--tamanho indice (para usar em list comprehension)
tamInd xs = length xs - 1

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

--id do navio dentro de naviosAlocadosBercoX
idsNavioAlocado naviosAlocados = [first4 ((second2 naviosAlocados)!!x) | x<-[0..tamInd (second2 naviosAlocados)]]
--id do berco dentro de naviosAlocadosBercoX
idBercoAlocado naviosAlocados = first3 (first2 naviosAlocados)
--lista de tempos dos navios usando naviosAlocadosBercoX
listaTemposNavios naviosAlocados infoPorto = infoPorto!!(fromInteger(idBercoAlocado naviosAlocados -1))
--selecionados de navios dentro de infoPorto
selecAlocados naviosAlocados infoPorto = [(listaTemposNavios naviosAlocados infoPorto)!!(fromInteger(x -1)) | x<-(idsNavioAlocado naviosAlocados)]
--tempo de atendimento para a fila de navios
tempoGastoNavAloc naviosAlocados infoPorto = sum (selecAlocados naviosAlocados infoPorto)

--lista de tempos de todos os berços
tempoGastoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [tempoGastoNavAloc (naviosAlocadosBercos!!x) infoPorto | x <-[0..tamInd naviosAlocadosBercos]]
--lista de temposOciosos de todos os berços
tempoOciosoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [ tempoOcioso (listaBercos!!x) (naviosAlocadosBercos!!x) infoPorto | x<-[0..tamInd listaBercos]]
                                            