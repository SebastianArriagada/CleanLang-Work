module Sebastian_arriagada_HW6
import StdEnv 



/*1. Given the list of integers, modify it in a following way:
I. Remove all numbers which are multiple of 3
II. Sort remaining list in descending order
III. Swap 1st and 2nd elements, 3rd and 4th, 5th and 6th and so on.
*/
 
shuffleSort :: [Int] -> [Int]
shuffleSort [] = []
shuffleSort lst = swampCuples (revSort (Not3Multiple lst))

revSort :: [Int] -> [Int]
revSort [] = []
revSort [x:xs] = (revSort [y \\ y <- xs | y > x]) ++ [x] ++ (revSort[ y \\ y <- xs | y < x])

swampCuples :: [Int] -> [Int]
swampCuples [] = []
swampCuples lst 
| (length lst ) == 1 = lst 
= [ lst!!1 , lst!!0] ++  swampCuples (drop 2 lst )

Not3Multiple :: [Int] -> [Int] 
Not3Multiple [] = []
Not3Multiple lst 
| (hd lst) rem 3 <> 0 = [hd lst] ++ Not3Multiple (tl lst)
= Not3Multiple (tl lst)

//Start = shuffleSort [4,3,2] // [2,4]
//Start = shuffleSort [4,1,3,2,5,6,7] // [5,7,2,4,1]
//Start = shuffleSort [3,6,3,9,12] // []
//Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
// Start = shuffleSort [] // []
 

// 2. Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime with m.
// Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
// Two integers a and b are coprime, if the only positive integer that divides (is a divisor of) both of them is 1.
 
phi :: Int -> Int
phi 1 = 1
phi num =  length( removeMembers [x \\ x <- [1 .. (num - 1)] | not (isAnyMember (removeMember 1 (getDivisors num)) (getDivisors x) ) ] (removeMember 1 (getDivisors num)))

getDivisors :: Int -> [Int]
getDivisors num 
| num == 1 = [1]
=  [a \\ a<- [1 .. (num-1)] | num rem a == 0 ]

isPrime :: Int -> Bool
isPrime x 
| x == 1 = True
= and [x rem a <> 0 \\ a<- [2 .. (x-1)]]
 
//Start = removeMembers (removeMember 1 (getDivisors 10))
//Start = getDivisors 10
//Start = phi 1 // 1
//Start = phi 10 // 4
//Start = phi 12414 // 4136
//Start = phi 100 // 40
//Start = phi 1000000 // 400000 -> it take a too long time, but return the correct result 
 

// 3.
// Write function that takes String as input and removes vowels from it

removeVowels :: String -> String
removeVowels str = (removeVowelsAux str 0)

removeVowelsAux :: String Int -> String
removeVowelsAux "" 0 = ""
removeVowelsAux str pos
| pos < (size str) && ((select str pos) <> 'a') && ((select str pos) <> 'e') && ((select str pos) <> 'i') && ((select str pos) <> 'o') && ((select str pos) <> 'u')  = toString(select str pos) +++ (removeVowelsAux str (pos + 1))
| pos < (size str) = removeVowelsAux str (pos + 1)
= ""

 /*

removeVowels str
| isMember "a" str = removeVowels (removeMember "a" str)
| isMember "e" str = removeVowels (removeMember "e" str)
| isMember "i" str = removeVowels (removeMember "i" str)
| isMember "o" str = removeVowels (removeMember "o" str)
| isMember "u" str = removeVowels (removeMember "u" str)
= str

*/



//Start = (select "hola" 2) <> 'l'
//Start = removeVowels "Xola"// "Xl"
//Start = removeVowels "Functional Programming" // "Fnctnl Prgrmmng"
//Start = removeVowels "Clean is the best" // "Cln s th bst"
//Start = removeVowels "Not really" // "Nt rll"
//Start = removeVowels "" // ""
//Start = removeVowels "N vwls hr" // "N vwls hr"