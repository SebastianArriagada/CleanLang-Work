module test_practice
import StdEnv

// 1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.

f1 :: Int -> Int
f1 num
| length lst  > 1 = abs(lst!!1)
= 0
where
    lst = (f1Aux num)

f1Aux :: Int -> [Int]
f1Aux num
| num == 0 = []
= [num rem 10 ] ++ f1Aux (num / 10)


//Start = f1 1234 //3

//Start = f1 5 //0

//Start = f1 -5564 //6


// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.

// Return 0 for an empty list.

f2 :: [Int] -> Int
f2 lst 
| lst == [ ] = 0
= foldr (\a b =   b  - a  ) (hd lst) ( tl lst)

//Start = f2 [10,1,2,3] //4

//Start = f2 [1,2,3,4] //-8

//Start = f2 [1000,500,250,125] //125

//Start = f2 [] //0


// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

f3 :: Int -> [Int]
f3 num 
| isPrime num = [1,num]
=  filter (\x = ( num rem x == 0)  ) (primeList num)

primeList :: Int -> [Int]
primeList num = filter isPrime [1 .. num] 

isPrime :: Int -> Bool
isPrime x = and [ x rem a <> 0 \\ a <- [2 .. (x - 1 )] ] 

//Start = primeList 36
//Start = f3 36 //[1,2,3]
//Start = f3 524287  //[1,524287]
//Start = f3 0 //[]


// 4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.

f4 :: [(Int, Int)] -> [(Int, Int)]
f4 lst
| lst == [] = []
| isEven suma = [( snd (hd lst), fst (hd lst) )] ++ f4 (tl lst)
= [hd lst]  ++ f4 (tl lst)
where 
    suma = fst (hd lst) + snd (hd lst)

//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]


// 5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

f5 :: [Int] -> [[Int]]
f5 lst = map multX5 lst

multX5 :: Int -> [Int]
multX5 num = map ((*) num) [1..5]

//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]

//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]


// 6. Given an integer n, find the minimal k such that

// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

// In other words, find the smallest factorial which is not less than n.

// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17

leastfactorial :: Int -> Int
leastfactorial num = prod [1 .. factorial num 1]

factorial :: Int Int -> Int
factorial num1 fact
| prod[1 .. fact] < num1 = factorial num1 (fact + 1)
= fact 


//Start = leastfactorial 17 // 24

//Start = leastfactorial 1 // 1

//Start = leastfactorial 5 // 6

//Start = leastfactorial 25 // 120


// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

f7 :: [Int] -> Bool
f7 lst
| length lst == 1 && isOdd (lst!!0 )= True
| length lst == 2 && isOdd (lst!!0) &&  isEven (lst!!1)  = True
| length lst == 3 && isOdd (lst!!0) &&  isEven (lst!!1) && isOdd (lst!!2) = True
| length lst > 3 && isOdd (lst!!0) &&  isEven (lst!!1) = f7 (drop 2 lst)
= False

//Start = f7 [1..10] //True

//Start = f7 [1,2,3] //True

//Start = f7 [2,3,4] //False

//Start = f7 [1,3,4,5] //False

//Start = f7 [1,2,3,4,6,7] //False

//Start = f7 [] //False


// 8. Write a function that removes consecutive duplicates in a list.

//f8 :: [Int] -> [Int] 

//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]

//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]

//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 


// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]
f9 tupleList = f9Aux (fst3 tupleList) (snd3 tupleList) (thd3 tupleList)

f9Aux :: [Int] [Int] [Int] -> [(Int,Int,Int)]
f9Aux lst1 lst2 lst3 
| lst1 == [] = []
| length lst1 == 2 = zip3 (hd lst1)  (hd lst2)  (hd lst3)
= zip3 (hd lst1)  (hd lst2)  (hd lst3) ++ f9Aux (tl lst1)  (tl lst2)  (tl lst3)

zip3 :: Int Int Int  -> [(Int,Int,Int)]
zip3 num1 num2 num3 
| num1 * num2 == num3 = [(num1,num2,num3)]
= []

//Start =  ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10]) 
//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]


// 10. Write a function that checks if a number is a Mersenne Prime.

// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1

f10 :: Int -> Bool
f10 num 
| ((findNumber num 1) - 1 ) == num = True
=False

findNumber :: Int Int-> Int
findNumber num pot
| (2 ^ pot) < num = 0 + findNumber num (pot + 1)
= (2 ^ pot)


//Start = f10 7 //True

//Start = f10 1 //False

//Start = f10 (~235) //False

//Start = f10 2147483647 //True

//Start = f10 0 //False