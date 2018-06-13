module ModuloNListas where

--Primero múdulo. Aqui serão guardadas as funções que não dizem respeito a listas.

filter xs pred = [x|x<-xs,pred]

--funções seletoras
first4 (x,y,z,w) = x
second4 (x,y,z,w) = y
third4 (x,y,z,w) = z
fourth4 (x,y,z,w) = w

first3 (x,y,z) = x
second3 (x,y,z) = y
third3 (x,y,z) = z

--tempo do navio x no berco y
tempoNavio navio berco infoPorto = (infoPorto!!(first3 berco -1))!!(first4 navio -1)

