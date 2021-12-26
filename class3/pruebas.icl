module pruebas
import StdEnv

g[x, y : z]= x + y

sum1 x
| x == [] = 0
= hd x + sum1 (tl x)

length1 

//  Start = g[1,2,3,4,5]
Start = sum1 [1..5]