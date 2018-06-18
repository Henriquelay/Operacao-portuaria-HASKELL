-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

-- Script principal. Aqui será guardado as funções que o trabalho pede apenas, entre poucas outras coisas.
import ModuloNListas
import ModuloListas
import Data.List

type Navio = (Int,Int,Int,Int)  {- ID, chegada, partida, quantidade -}
type Berco = (Int,Int,Int)      {- ID, abertura, fechamento -}
--type Porto = [Int..] {- [berço1[TempoNavio1 TempoNavio2...],berço2[Temponavio1, TempoNavio2...]] -}


{- Dados copiados do exemplo do PDF, a propósito de testes -}
tlistaNavios::[(Int,Int,Int,Int)]
tlistaNavios = [(1 , 5 , 16 , 30) , (2 , 6 , 18 , 30) , (3 , 3 , 12 , 50) , (4 , 4 , 22 , 50) , (5 , 11 , 20 , 80)]
tlistaBercos::[(Int,Int,Int)]
tlistaBercos = [(1 , 4 , 20) , (2 , 3 , 18)]
tinfoPorto::[[Int]]
tinfoPorto = [[1 , 6 , 4 , 4 , 6], [2 , 0 , 1 , 0 , 5]]
tnaviosAlocadosBerco1 = ((1 , 4 , 20) , [(4 , 4 , 22 , 50) , (5 , 11 , 20 , 80)])
tnaviosAlocadosBerco2 = ((2 , 3 , 18) , [(3 , 3 , 12 , 50) , (1 , 5 , 16 , 30)])
tnaviosAlocadosBerco = [tnaviosAlocadosBerco1 , tnaviosAlocadosBerco2]
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

--atendido:: Navio -> Berco -> [a] -> Bool {- \/ \/ \/ \/ seleciono o tempo do berço a esse navio correto da lista de informações do porto -}
atendido navio berco infoPorto = if tempoCargaNavio navio berco infoPorto <= tempoNavio navio && tempoCargaNavio navio berco infoPorto /= 0
                                then True
                                else False

filaNavios listaNavios = mergesortFila listaNavios

tempoOcioso berco naviosAlocados infoPorto = if tempoBerco berco <= tempoGastoNavAloc naviosAlocados infoPorto
                                                then 0
                                                else tempoBerco berco - tempoGastoNavAloc naviosAlocados infoPorto
--assumindo que o berco digitado manualmente corresponde ao berco de naviosAlocados

bercoOcioso listaBercos naviosAlocadosBercos infoPorto = map (+1) (elemIndices (maximum (tempoOciosoTdsBercos listaBercos naviosAlocadosBercos infoPorto)) (tempoOciosoTdsBercos listaBercos naviosAlocadosBercos infoPorto))
--lista de temposOciosos de todos os berços --teve de vir pra essa lista pois usa ''tempoOcioso'' e os módulos não importam funções da Main
tempoOciosoTdsBercos listaBercos naviosAlocadosBercos infoPorto = [ tempoOcioso (listaBercos!!x) (naviosAlocadosBercos!!x) infoPorto | x<-(indices listaBercos)] 

naviosCandidatosBerco listaBercos listaNavios infoPorto
                                                        | null listaBercos || null listaNavios = []
                                                        | otherwise = [(listaBercos!!x,naviosCandidatos (listaBercos!!x) listaNavios infoPorto)|x<-(indices listaBercos)]

--navios candidatos de uma lista ao berco x --teve de vir pra essa lista pois usa ''atendido'' e os módulos não importam funções da Main
naviosCandidatos berco listaNavios infoPorto = [listaNavios!!y |y<-(indices listaNavios), atendido (listaNavios!!y) berco infoPorto]              

insereNavioBerco navio naviosAlocados infoPorto = if tempoCargaNavio navio <= tempoNavio navio && notElem navio (naviosAlocadosSemBerco naviosAlocados) && tempoOcioso (first2 naviosAlocados) naviosAlocados infoPorto >= tempoNavio navio
                                                        then (idBerco naviosAlocados, fourth4 navio + sum (map fourth4 (naviosAlocadosSemBerco naviosAlocados)),navio : (naviosAlocadosSemBerco naviosAlocados))
                                                        else (0,0,[0])