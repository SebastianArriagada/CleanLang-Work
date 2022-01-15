
module pt9gr6

import StdEnv 

/*
Create an * instance of lists such that list1 * list2 will give a
list of pairwise product of the two lists and if the length
of one list is greater than the other one just add the
remaining elements to the end of the new list
*/

instance * [Int]
where
     (*) lst1 lst2 =  [ x * y  \\ x<-lst1 & y<-lst2 ] ++ drop (minNum lst1 lst2) lst1 ++ drop (minNum lst1 lst2) lst2 
    
minNum :: [Int] [Int] -> Int 
minNum [] [] = 0
minNum lst1 lst2
| length lst1 > length lst2 = length lst2 
= length lst1 

Start = [1,2] * [3,4,5,6,0] // [3,8,5,6,0]
//Start = [1,2,3,1,3,12,312] * [2,3] // [2,6,3,1,3,12,312]
