module homework3
import StdEnv

/*
1/ Given three Integers, create a list which contains powers of the first integer starting from second integer to third integer.
(Assume the second integer is non-negative and third integer is equal to or greater than second integer)

example: powers 2 3 7 = [2^3, 2^4, 2^5, 2^6]
*/

powers::Int Int Int -> [Int]
powers base start end
| start == end = []
= [base^start] ++ powers base (start+1) end


//Start = powers 2 3 7 // [8,16,32,64]
//Start = powers 3 0 9 // [1,3,9,27,81,243,729,2187,6561]
//Start = powers 5 4 4 // []
//Start = powers 7 1 15 // [7,49,343,2401,16807,117649,823543,5764801,40353607,282475249,1977326743,13841287201,96889010407,678223072849]

/*
2/ Given a list of lists of Integers and an Integer, 
mulitply every elements in the lists by the Integer and return a list of sums of the even numbers after mulitplication.
*/



sums :: [[Int]] Int -> [Int]
sums listOfList num
| listOfList == [] = []
| length listOfList < 3 = [ sum ( filter isEven ( map ((*) num) (hd listOfList) ) ),  sum ( filter isEven (map ((*) num) (flatten (tl listOfList))) )]
= [ sum ( filter isEven ( map ((*) num) (hd listOfList) ) ) ] ++ sums (tl listOfList) num 

//Start = sums [[1,2,3],[4,5,6],[7,7,7]] 2 // [12,30,42]
//Start = sums [[-2,2],[3,3,4,5,7],[10,1,2]] 3 // [0,12,36]
//Start = sums [] 4 // []
//Start = sums [[8,8,3],[4,5,6,9],[1,1,6],[5]] 5 // [80,50,30,0]

/*
3/ Given a list of integers, write a function which iterates through every element and return list of lists of Fibonnaci sequence as shown in example.
The elements of the list indicates how many Fibonacci numbers are in the corresponding sublist and the Fibonacci sequence will continue in the next sublist.
(Assume there is no negative integer in the list)

example: fibList [3,2,1,2] = [[1,1,2],[3,5],[8],[13,21]]
		 fibList [2,5,3] = [[1,1],[2,3,5,8,13],[21,34,55]]
*/

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fibList::[Int] -> [[Int]]
fibList lista
|lista == [] = []
= cutList lista (map ((fib)) [0,1 .. (sum lista) - 1])

cutList :: [Int] [Int] -> [[Int]]
cutList listaNum listaFib
| listaNum == [] = []
= [  take (hd listaNum) listaFib  ] ++  cutList (tl listaNum) ( drop (hd listaNum) listaFib)  

//Start = fibList [4,5,3,2] // [[1,1,2,3],[5,8,13,21,34],[55,89,144],[233,377]]
//Start = fibList [] // []
//Start = fibList [10,5] // [[1,1,2,3,5,8,13,21,34,55],[89,144,233,377,610]]


