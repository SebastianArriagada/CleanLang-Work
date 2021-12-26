module Homework2
import StdEnv 

// Sebastian Arriagada Silva, NeptunID HSO14Z

/*
1- Given a list consisting of 4 real values (first two values are one of the vertices of a rectangle x and y coordinate,
third is the rectangle's width and the fourth is the rectangle' height (width and height can be negative too). For example,
in case of  x =1, y=5, width=6, height=-2,
the bottom left vertex coordinates will be 1 and 3.
Define topLeft, topRight , bottomRight, and bottomLeft functions returning
the corresponding vertex of the rectangle as a list consisting of  x , y represinting the point of the resulting function.
*/

topLeft :: [Real] -> [Real]
topLeft point
| (point!!2 > 0.0) && (point!!3 < 0.0) = [point!!0, point!!1]
| (point!!2 > 0.0) && (point!!3 > 0.0) = [point!!0, (point!!1 + point!!3)]
| (point!!2 < 0.0) && (point!!3 > 0.0) = [ (point!!0 + point!!2 ), (point!!1 + point!!3) ]
| (point!!2 < 0.0) && (point!!3 < 0.0) = [ (point!!0 + point!!2 ), point!!1]
= [0.0,0.0]

topRight :: [Real] -> [Real]
topRight point
| (point!!2 > 0.0) && (point!!3 < 0.0) = [ (point!!0 + point!!2 ), point!!1]
| (point!!2 > 0.0) && (point!!3 > 0.0) = [(point!!0 + point!!2 ), (point!!1 + point!!3)]
| (point!!2 < 0.0) && (point!!3 > 0.0) = [ point!!0 , (point!!1 + point!!3) ]
| (point!!2 < 0.0) && (point!!3 < 0.0) = [ point!!0 , point!!1]
= [0.0,0.0]

bottomLeft:: [Real] -> [Real]
bottomLeft point
| (point!!2 > 0.0) && (point!!3 < 0.0) = [ point!!0, (point!!1 + point!!3) ]
| (point!!2 > 0.0) && (point!!3 > 0.0) = [point!!0, point!!1 ]
| (point!!2 < 0.0) && (point!!3 > 0.0) = [ (point!!0 + point!!2 ), point!!1 ]
| (point!!2 < 0.0) && (point!!3 < 0.0) = [ (point!!0 + point!!2 ), (point!!1 + point!!3)]
= [0.0,0.0]

bottomRight:: [Real] -> [Real]
bottomRight point
| (point!!2 > 0.0) && (point!!3 < 0.0) = [ (point!!0 + point!!2 ), (point!!1 + point!!3) ]
| (point!!2 > 0.0) && (point!!3 > 0.0) = [(point!!0 + point!!2 ), point!!1 ]
| (point!!2 < 0.0) && (point!!3 > 0.0) = [ point!!0, point!!1 ]
| (point!!2 < 0.0) && (point!!3 < 0.0) = [ point!!0 , (point!!1 + point!!3)]
= [0.0,0.0]



//Start = topLeft[3.0 , 5.0 , 1.0 , -7.0] ++  topRight[3.0 , 5.0 , 1.0 , -7.0] ++ bottomLeft [3.0 , 5.0 , 1.0 , -7.0] ++ bottomRight[3.0 , 5.0 , 1.0 , -7.0]  
          //[3.0 ,5.0 , 4.0 , 5.0  , 3.0 , -2.0 ,  4.0 ,  -2.0  ]
//Start = topLeft[2.0,8.0,-4.0,-7.0] ++  topRight[2.0,8.0,-4.0,-7.0] ++ bottomLeft[2.0,8.0,-4.0,-7.0] ++ bottomRight[2.0,8.0,-4.0,-7.0]
         // [-2.0,8.0,2.0,8.0,-2.0,1.0,2.0,1.0]
//Start = topLeft[1.0,6.0,4.0,2.0] ++  topRight[1.0,6.0,4.0,2.0] ++ bottomLeft[1.0,6.0,4.0,2.0] ++ bottomRight[1.0,6.0,4.0,2.0]
		  // [1.0,8.0,5.0,8.0,1.0,6.0,5.0,6.0]
//Start = topLeft[1.0,6.0,-4.0,2.0] ++  topRight[1.0,6.0,-4.0,2.0] ++ bottomLeft[1.0,6.0,-4.0,2.0] ++ bottomRight[1.0,6.0,-4.0,2.0]
		  // [-3.0,8.0,1.0,8.0,-3.0,6.0,1.0,6.0]





/*2. Given a list of numbers, multiply every even number of the list by 2,
and every odd number of the list by 3*/

multiply :: [Int] -> [Int]
multiply list
|list == [] = list
|hd list rem 2 == 0 = [hd list * 2] ++ multiply(tl list)
= [hd list * 3] ++ multiply(tl list)


//Start = multiply [14, 22, 45, 56] // [28, 44, 135, 112]
//Start = multiply [13, 27, 44] // [39, 81, 88]
//Start = multiply [] // []
 
 
 
 
  
/*
3. Given two lists of integers of the same length, 
check if the elements on the same positions in the two lists are of the same property: both even or both odd.
Return True for empty lists. */ 

same :: [Int] [Int] -> Bool
same list1 list2
| (list1 == []) && (list2 == []) = True
| (hd list1 rem 2 == 0) && (hd list2 rem 2 == 0) = same (tl list1) (tl list2)
| (hd list1 rem 2 <> 0) && (hd list2 rem 2 <> 0) = same (tl list1) (tl list2)
= False

//Start = same [1,2,3] [2,4,6] // False
//Start = same [1,2,3,4] [3,8,5,12] // True
//Start = same [] [] // True


 
 
 