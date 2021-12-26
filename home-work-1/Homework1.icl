module Homework1
import StdEnv 

// Sebastian Arriagada Silva, NeptunID HSO14Z

/*
1- write GetLastPositive function which returns the last digit of the number if its positive 
and -1 if the number is negative
*/

GetLastPositive :: Int -> Int 
GetLastPositive number 
| number < 0 = -1
| number <= 9 = number
= ExtractLastNumber number

ExtractLastNumber :: Int -> Int
ExtractLastNumber number 
| Result number < 10 = Result number
= Result2 number

Result :: Int -> Int
Result number = ( MultiplayBy10(  1.0 - ( DividedBy10AndDelateDecimal( number ) - DividedBy10(number) ) ) )

Result2 :: Int -> Int
Result2 number =  (MultiplayBy10( (  DividedBy10(number) - DividedBy10AndDelateDecimal( number ) ) ) )

DividedBy10 :: Int -> Real
DividedBy10 number = ( (toReal(number) ) /10.0)

DividedBy10AndDelateDecimal :: Int -> Real
DividedBy10AndDelateDecimal number = ( toReal ( toInt ( DividedBy10(number) ) ) )

MultiplayBy10 :: Real -> Int
MultiplayBy10 number = toInt( number * 10.0 )

//Start = GetLastPositive 5856 // 6  
//Start = GetLastPositive 689255 // 5
//Start = GetLastPositive 0 // 0
//Start = GetLastPositive 8 // 8
//Start = GetLastPositive -8554 // -1
//Start = GetLastPositive -1 // -1
//Start = GetLastPositive 1112 // 2
//Start = GetLastPositive 10 // 0
 

 

 

 

/*
2- Given two real numbers decide whether  the sum of the two numbers after the decimal points  of the two numbers is Even or not
assume there is only one number after the decimal point  
Hint : You can use (toInt) function .  

 

*/

 


IsEvenDecimal :: Real Real -> Bool
IsEvenDecimal x y = IsEven(extractDecimal x + extractDecimal y)

IsEven :: Real -> Bool
IsEven y
| (toInt(y*10.0)) rem  2 == 0 = True
= False

extractDecimal :: Real -> Real
extractDecimal num = num - toReal ( toInt( num)  )


//Start = IsEvenDecimal 5.3 4.6 // False
//Start = IsEvenDecimal 4.1 4.6 // False
//Start = IsEvenDecimal 1.2 6.6 // True






// 3-  Write a function that will take a digit (Int)

 

// and return the respective word for it (String).
// For example input of 1 should output One; input of 0 should output Zero; input of 5 should output Five.
// Anything that is not the digit (0-9) should output "Not a digit"

 

 

digit_to_string :: Int -> String

digit_to_string num
| num == 0 = "One"
| num == 1 = "One"
| num == 2 = "Two"
| num == 3 = "Three"
| num == 4 = "Four"
| num == 5 = "Five"
| num == 6 = "Six"
| num == 7 = "Seven"
| num == 8 = "Eight"
| num == 9 = "Nine"
= "Not a digit"

//Start = digit_to_string 4 //"Four"
//Start = digit_to_string 0 //"Zero"
//Start = digit_to_string 5 //"Five"
//Start = digit_to_string 10 //"Not a digit"
//Start = digit_to_string -1 //"Not a digit"
//Start = digit_to_string 42 //"Not a digit"