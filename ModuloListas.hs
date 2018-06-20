-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.
module ModuloListas where
import ModuloNListas
import Data.List                           

--tamanho indice (para usar em list comprehension)
tamInd xs = length xs - 1

indices xs = [0..tamInd xs]

--mergesort modificado para comparar o segundo elemento de uma tupla (apenas o intercala foi alterado)
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
idsNavioAlocado naviosAlocados = [first4 ((naviosAlocadosSemBerco naviosAlocados)!!x) | x<-(indices (naviosAlocadosSemBerco naviosAlocados))]
--lista de tempos dos navios usando naviosAlocadosBercoX
listaTemposNavios naviosAlocados infoPorto = infoPorto!!(fromInteger(idBerco naviosAlocados -1))
--selecionados de navios dentro de infoPorto
selecAlocados naviosAlocados infoPorto = [(listaTemposNavios naviosAlocados infoPorto)!!(fromInteger(x -1)) | x<-(idsNavioAlocado naviosAlocados)]
--tempo de atendimento para a fila de navios
tempoGastoNavAloc naviosAlocados infoPorto = sum (selecAlocados naviosAlocados infoPorto)

--lista de tempos de todos os berços
tempoGastoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [tempoGastoNavAloc (naviosAlocadosBercos!!x) infoPorto | x <-(indices naviosAlocadosBercos)]

