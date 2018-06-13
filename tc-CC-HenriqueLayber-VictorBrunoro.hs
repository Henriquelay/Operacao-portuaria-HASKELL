-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --
-- Script principal
import Modulos

type Navio = (Int,Int,Int,Int)  {- ID, chegada, partida, quantidade -}
type Berco = (Int,Int,Int)      {- ID, abertura, fechamento -}
{- infoPorto [[Int, Int...], [Int, Int...]...] -}{- [berço1[TempoNavio1 TempoNavio2...],berço2[Temponavio1, TempoNavio2]...] -}


{- Dados copiados do exemplo do PDF, a propósito de testes -}
tlistaNavios = [(1,5,16,30), (2,6,18,30), (3,3,12,50), (4,4,22,50), (5,11,20,80)]
tlistaBercos = [(1,4,20), (2,3,18)]
tinfoPorto = [[1, 6, 4, 4, 6], [2, 0, 1, 0, 5]]
tnaviosAlocadosBerco1 = ((1,4,20), [(4,4,22,50), (5,11,20,80)])
tnaviosAlocadosBerco2 = ((2,3,18), [(3,3,12,50), (1,5,16,30)])
tnaviosAlocadosBerco = [tnaviosAlocadosBerco1, tnaviosAlocadosBerco2]
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

--atendido:: Navio -> Berco -> [a] -> Bool {- \/ \/ \/ \/ seleciono o tempo do berço a esse navio correto da lista de informações do porto -}
atendido navio berco infoPorto = if tempoNavio navio berco infoPorto >= abs(third4 navio - second4 navio) && tempoNavio navio berco infoPorto /= 0
                                then True
                                else False