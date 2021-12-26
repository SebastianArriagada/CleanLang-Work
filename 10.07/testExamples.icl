
module testExamples
import StdEnv

/* Crear una funcion que filtre los numeros divisibles por 10, pero no por 4 de una lista */


removeDiv10No4 :: [Int] -> [Int]
removeDiv10No4 lista = filter (\x = ( x rem 10 == 0 && x rem 4 <> 0)) lista

//Start = removeDiv10No4 [ 10, 20 .. 100]

/* escribir una funcion que convierta lis numeros binarios a decimales */

/*
binaryToDecimal :: Int -> Int
binaryToDecimal num = calculate (split num) [length (split num ), (length (split num ) -1) .. 0]

calculate :: [Int] [Int] -> Int
calculate lista1 lista2
| lista1 == [] = 0
= conv (hd lista1) (hd lista2) + calculate (tl lista1) (tl lista2)

conv :: Int Int -> Int
conv base potencia = base * (2) ^ potencia

split :: Int -> [Int]
split x 
| x == 0 = []
= split (toInt x/10) ++ [x rem 10]
*/

//binaryToDecimal :: Int -> Int
//binaryToDecimal binRep = binaryToDecimalAux binRep 1


//Start = binaryToDecimal 101010


/* Given a list of list, for each list, extract the smaller, medium and hight element */

extract :: [[Int]] -> [(Int, Int, Int)]
extract listOfList = map takeNumbers listOfList

takeNumbers :: [Int] -> (Int, Int, Int)
takeNumbers lst = (mn, md, mx)
where
    sortedList = sort lst
    mn = hd sortedList
    mx = last sortedList
    md = sortedList!!(( length sortedList) / 2 )

//Start = extract [[ 1 .. 9], [10 .. 20], [ 15 .. 22] ]

/* una funcoin que retorne el maximo de cada lista, que sea menor a un numero dado */

//axLessThanN :: [[Int]] Int -> [Int]
//maxLessThanN lst num = map maxLessThanNAux (lst num)

maxLessThanNAux :: [Int] -> Int
maxLessThanNAux lst = last (sort lst)

maxLessThanNAux :: [Int] Int -> Int
maxLessThanNAux lst num = last ( sort ( filter (\x = ( x < num ) )lst )  )
//maxLessThanNAux lst num = filter (\x = ( x < num ) )lst 

Start = maxLessThanNAux [ 1 .. 10] 8