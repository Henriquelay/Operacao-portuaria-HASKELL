-- {[== -- == -- == -- == -- = -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

--Primero múdulo. Aqui serão guardadas as funções que não dizem respeito a listas.
module ModuloNListas where

type Navio = (Int,Int,Int,Int)  {- ID, chegada, partida, quantidade -}
type Berco = (Int,Int,Int)

--funções seletoras
first4::Navio -> Int
first4 (x,y,z,w) = x
second4::Navio -> Int
second4 (x,y,z,w) = y
third4::Navio -> Int
third4 (x,y,z,w) = z
fourth4::Navio -> Int
fourth4 (x,y,z,w) = w

first3::Berco -> Int
first3 (x,y,z) = x
second3::Berco -> Int
second3 (x,y,z) = y
third3::Berco -> Int
third3 (x,y,z) = z

--map tuplas
tupMap2 f (x,y) = (f x, f y)
tupMap3 f (x,y,z) = (f x, f y, f z)
tupMap4 f (x,y,z,w) = (f x, f y, f z, f w)

--tempo do navio x no berco y, dentro de infoPorto
tempoCargaNavio:: Navio -> Berco -> [[Int]] -> Int
tempoCargaNavio navio (x,y,z) infoPorto = (infoPorto!!(x -1))!!(first4 navio -1)

--tempo de trabalho do berco
tempoBerco:: Berco -> Int
tempoBerco berco = abs(third3 berco - second3 berco)

--tempo de trabalho do Navio
tempoNavio:: Navio -> Int
tempoNavio navio = abs(third4 navio - second4 navio)

--id do berco em naviosAlocados
idBerco::(Berco,[Navio]) -> Int
idBerco naviosAlocados = first3(fst naviosAlocados)

--selecionar os navios alocados de naviosAlocadosBerco
naviosAlocadosSemBerco:: (Berco,[Navio]) -> [Navio]
naviosAlocadosSemBerco naviosAlocados = snd naviosAlocados