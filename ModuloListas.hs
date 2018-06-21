-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.
module ModuloListas where
import ModuloNListas
import Data.List      

type Navio = (Int,Int,Int,Int)  {- ID, chegada, partida, quantidade -}
type Berco = (Int,Int,Int)      {- ID, abertura, fechamento -}

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

--ids dos navios dentro de naviosAlocadosBercoX
idsNavioAlocado::(Berco,[Navio]) -> [Int]
idsNavioAlocado naviosAlocados = map (first4) naviosAlocados
--lista de tempos carga dos navios usando naviosAlocadosBercoX
listaTemposNavios::(Berco,[Navio]) -> [[Int]] -> [Int]
listaTemposNavios naviosAlocados infoPorto = infoPorto!!(idBerco naviosAlocados -1)
--selecionados de navios dentro de infoPorto
selecAlocados::(Berco,[Navio]) -> [[Int]] -> [Int]
selecAlocados naviosAlocados infoPorto = [(listaTemposNavios naviosAlocados infoPorto)!!(x -1) | x<-(idsNavioAlocado naviosAlocados)]
--tempo de atendimento para a fila de navios
tempoGastoNavAloc::(Berco,[Navio]) -> [[Int]] -> Int
tempoGastoNavAloc naviosAlocados infoPorto = sum (selecAlocados naviosAlocados infoPorto)

--lista de tempos de todos os berços
tempoGastoTdsBercos::[Berco] -> (Berco,[Navio]) -> [[Int]] -> [Int]
tempoGastoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [tempoGastoNavAloc (naviosAlocadosBercos!!x) infoPorto | x <-(indices naviosAlocadosBercos)]

