
module quiz1
import StdEnv

/* Given the list of unique numbers, return sum of all even numbers.
 * Examples:
 * [] -> 0
 * [1,3,17] -> 0
 * [2] -> 2
 * [2,120] -> 122
 * [1,2,3,4] -> 6
 * [12,8,1,2,7] -> 22
 */

// TODO
evenSum:: [Int] -> Int
evenSum x
|x == [] = 0
| toInt(hd x) rem 2 == 0 = hd x + evenSum (tl x)
| otherwise = 0 + evenSum (tl x)


Start =  evenSum [12,8,1,2,7] //evenSum [1,3,17]
