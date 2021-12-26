
module code
import StdEnv

/*

Given the list of strings. Find sum of lengthes of such strings
that contain at least 5 characters.
*/


f :: [String] -> Int
f lista
| lista == [] = 0
| countLength (toInt(hd lista)) > 4 = countLength (toInt(hd lista)) + f (tl lista)
= f (tl lista)

intToList :: Int -> [Int]
intToList x
| x < 10 = [x]
=(intToList (x/10)) ++ [x rem 10]

countLength:: Int -> Int
countLength x = length ( intToList x)

// Start = f ["aa","fljhasbf","bcde"] // 8
//Start = f ["123456789", "123", "12345678", "1234", "12345"] // 22
// Start = f [] // 0

Start = size("hola") 