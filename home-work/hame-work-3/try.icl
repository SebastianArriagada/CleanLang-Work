module try
import StdEnv
 
sums :: [[Int]] Int -> [Int]
sums listOfList num 
| listOfList == [] = []
= map ((*)2) (flatten listOfList)


Start = take 3 [10,3, 4 ,6,7 ,8,4] 
//Start = sums [[1,2,3],[4,5,6],[7,7,7]] 2 // [12,30,42]
//Start = sums [[-2,2],[3,3,4,5,7],[10,1,2]] 3 // [0,12,36]
