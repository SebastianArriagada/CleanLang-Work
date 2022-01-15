module quiz2
import StdEnv

/*
Given a list of positive integer numbers, extract the middle digit
of each number
e.g: [1242, 55341, 231, 23 ,2] = [4,3,3,2,2]
*/

intToList :: Int -> [Int]
intToList x
| x < 10 = [x]
=(intToList (x/10)) ++ [x rem 10]

getLength :: [Int] -> Int
getLength x = length(x)

midDig :: [Int] -> [Int]
midDig x
| x == [] = x
= [  intToList (hd x ) !! (getLength(intToList (hd x )) / 2) ] ++ midDig (tl x)

//Start = getLength (intToList 434242)


Start = midDig [1242, 55341, 231, 23 ,2] // [4,3,3,2,2]
//Start = midDig [25551, 18645, 424] // [5,6,2]
//Start = midDig [] // [] 
