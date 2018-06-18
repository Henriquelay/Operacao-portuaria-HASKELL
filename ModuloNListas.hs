-- {[== -- == -- == -- == -- == -- == -- == -- == -- == -- == -- == -- ==]} --
-- {(Trabalho computacional, UFES PROG1 2018/1 Profa Maria Claudia Silva Boeres)} --
-- {(Dupla formada por Henrique Coutinho Layber e Vitor Brunoro)} --

--Primero múdulo. Aqui serão guardadas as funções que não dizem respeito a listas.
module ModuloNListas where

--funções seletoras
first4 (x,y,z,w) = x
second4 (x,y,z,w) = y
third4 (x,y,z,w) = z
fourth4 (x,y,z,w) = w

first3 (x,y,z) = x
second3 (x,y,z) = y
third3 (x,y,z) = z

first2 (x,y) = x
second2 (x,y) = y

--map tuplas
tupMap2 f (x,y) = (f x, f y)
tupMap3 f (x,y,z) = (f x, f y, f z)
tupMap4 f (x,y,z,w) = (f x, f y, f z, f w)

--tempo do navio x no berco y, dentro de infoPorto
tempoCargaNavio navio berco infoPorto = (infoPorto!!(first3 berco -1))!!(first4 navio -1)

--tempo de trabalho do berco
tempoBerco berco = abs(third3 berco - second3 berco)

--tempo de trabalho do Navio
tempoNavio navio = abs (third4 navio - second4 navio)

--id do berco em naviosAlocados
idBerco naviosAlocados = first3(first2 naviosAlocados)

--selecionar os navios alocados de naviosAlocadosBerco
naviosAlocadosSemBerco naviosAlocados = second2 naviosAlocados