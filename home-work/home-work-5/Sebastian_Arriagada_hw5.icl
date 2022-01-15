module Sebastian_Arriagada_hw5
import StdEnv 


/*1. Given the list of points and a distance. Each point is represented with tuple, containing X and Y coordinates
in 2D plane. Return how many pairs of points are there so that, distance between them is equal to the given number.
*/
// You can use this formula to find the distance between two points d = square root of ((x_2-x_1)�+(y_2-y_1)�) 


pointDistance :: [(Int,Int)] Int -> Int
pointDistance lst num = (sum [ 1 \\ (x1,y1) <- lst, (x2,y2) <- lst | ( toReal(x2-x1)^2.0 + toReal(y2 - y1)^2.0 )^(0.5) == toReal(num) ]) /2


//Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 5 // 2
//Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 2 // 0
//Start = pointDistance [(3,4), (3,8), (7,8)] 4 // 2
//Start = pointDistance [] 3 // 0
//Start = pointDistance [(1,1)] 2 // 0





/*2. Write a function that takes a list of integers and gives back a tuple that contains:
(the integer in the list, a boolean value)
the boolean value tells if when cutting the integer in half it consists of
the same number, e,g, 2020 -> 20 20 so it's true but 2008 -> 20 08 it's not.
*/


toTuple :: [Int] -> [(Int, Bool)]
toTuple lst 
| lst == [] = []
= [(hd lst, toTupleAux (hd lst))] ++ toTuple ( tl lst) 


toTupleAux :: Int -> Bool
toTupleAux num 
| (splitNumber num)!!0 == (splitNumber num)!!1 = True 
=False 


splitNumber :: Int -> [Int]
splitNumber num 
| isEven lstLenght = [ jointNumber ( take midLstLenghtEven lst) 0, jointNumber ( drop midLstLenghtEven lst) 0]
=  [ jointNumber ( take midLstLenghtOdd lst) 0, jointNumber ( drop midLstLenghtOdd lst) 0]
where
     lst = fromIntToList num
     lstLenght = length (lst)
     midLstLenghtEven = (lstLenght)/2
     midLstLenghtOdd = (lstLenght)/2 + 1

jointNumber :: [Int] Int -> Int
jointNumber lst pot  
| lst == [] = 0
= (last lst) * 10^pot  + jointNumber (init lst) (pot + 1) 

fromIntToList :: Int -> [Int]
fromIntToList num 
| num == 0 = []
= fromIntToList (num/10) ++ [num rem 10]

//Start = toTuple [] // []
//Start = toTuple [100, 2020, 1919] // [(100,False),(2020,True),(1919,True)]
//Start = toTuple [312, 1001, 1010] // [(312,False),(1001,False),(1010,True)]



/*

3-Given two integer numbers a and b , filter out the perfect numbers in the interval [a..b] and 
generate a list of tuples such that every tuple contains the perfect number and the number of 
its digits . 
In number theory, a perfect number is a positive integer that is equal to the sum of its positive divisors , 
excluding the number itself. For instance, 6 has divisors 1, 2 and 3, and 1 + 2 + 3 = 6, so 6 is a perfect number . 


e.g perfect 5 30 = [(6,1) , (28,2)] because 6 is in the interval [5..30] and it is 
perfect number and the number of its digits is 1, similarly for 28.  

*/
perfect :: Int Int -> [(Int, Int)]
perfect num1 num2 = perfectAux (filter isPerfect [num1 .. num2 ])

perfectAux :: [Int] -> [(Int, Int)]
perfectAux lst 
|lst == [] = []
= [ (hd lst, length(fromIntToList (hd lst)))] ++ perfectAux (tl lst )

isPerfect :: Int -> Bool
isPerfect num = ( ( sum [ x \\ x <- [1 .. (num - 1) ] | (num rem x) == 0 ] ) == num)

//Start = isPerfect 7
//Start = perfect 5 30 //  [(6,1),(28,2)]
//Start = perfect 20 1000// [(28,2),(496,3)]
//Start = perfect 300 9000 // [(496,3),(8128,4)]  
//Start = perfect 100 200 // [] 










