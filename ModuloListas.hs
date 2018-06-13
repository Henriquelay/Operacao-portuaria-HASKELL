module ModuloListas where

--Segundo múdulo. Aqui serão guardadas as funções que dizem respeito a listas.

--seleciona o menor elemento
menorElem xs
            | null (tail xs) || null xs = xs   
            | otherwise = if xs!!0 > xs!!(length xs -1)
                            then menorElem (tail xs)
                            else menorElem (init xs)