-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.
module ModuloListas where
import ModuloNListas
import Data.List

--seleciona o menor elemento
menorElem xs
            | null (tail xs) || null xs = xs   
            | xs!!0 == xs!!(tamInd xs) = (head xs) : (menorElem (tail xs))
            | otherwise = if xs!!0 > xs!!(tamInd xs)
                            then menorElem (tail xs)
                            else menorElem (init xs)

maiorElem xs
            | null xs || null (tail xs) = xs
            | xs!!0 == xs!!(tamInd xs) = (head xs) : (maiorElem (tail xs))
            | otherwise = if xs!!0 < xs!!(tamInd xs)
                            then maiorElem (tail xs)
                            else maiorElem (init xs)                            

--tamanho indice (para usar em list comprehension)
tamInd xs = length xs - 1

indices xs = [0..tamInd xs]

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
idsNavioAlocado naviosAlocados = [first4 ((second2 naviosAlocados)!!x) | x<-(indices (second2 naviosAlocados))]
--id do berco dentro de naviosAlocadosBercoX
idBercoAlocado naviosAlocados = first3 (first2 naviosAlocados)
--lista de tempos dos navios usando naviosAlocadosBercoX
listaTemposNavios naviosAlocados infoPorto = infoPorto!!(fromInteger(idBercoAlocado naviosAlocados -1))
--selecionados de navios dentro de infoPorto
selecAlocados naviosAlocados infoPorto = [(listaTemposNavios naviosAlocados infoPorto)!!(fromInteger(x -1)) | x<-(idsNavioAlocado naviosAlocados)]
--tempo de atendimento para a fila de navios
tempoGastoNavAloc naviosAlocados infoPorto = sum (selecAlocados naviosAlocados infoPorto)

--lista de tempos de todos os berços
tempoGastoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [tempoGastoNavAloc (naviosAlocadosBercos!!x) infoPorto | x <-(indices naviosAlocadosBercos)]

