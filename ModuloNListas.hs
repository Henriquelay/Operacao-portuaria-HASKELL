module ModuloNListas where

--Primero múdulo. Aqui serão guardadas as funções que não dizem respeito a listas.


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

--tempo do navio x no berco y
tempoNavio navio berco infoPorto = (infoPorto!!(first3 berco -1))!!(first4 navio -1)

--tempo de trabalho do berco
tempoBerco berco = abs(third3 berco - second3 berco)