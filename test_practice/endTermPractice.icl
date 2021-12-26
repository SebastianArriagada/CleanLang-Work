module endTermPractice
import StdEnv 



::Human = { firstName::String, age::Int, height:: Int}

Rose::Human
Rose = {firstName = "Rose", age = 23, height = 172}
Jack::Human
Jack = { firstName="Jack",age=25,height=193 }
Emilia::Human
Emilia = {firstName="Emilia",age=15,height=160}
Leo::Human
Leo = {firstName="Leo",age=16,height=175}
Grace::Human
Grace = {firstName="Grace",age=35,height=165}
Harry::Human
Harry = {firstName = "Harry", age = 42, height = 180}
Emilia2::Human
Emilia2 = {firstName = "Emilia", age = 15, height = 180}



/*
    1. Create an instance of `==` for the Human record. Two people are equal if all their
    attributes are the same
*/


instance == Human
where
     (==) x y = (x.firstName == y.firstName) && (x.age == y.age) && (x.height == y.height)



//Start = Leo == Rose // False
//Start = Harry == Harry // True
//Start = Emilia == Emilia2 // False




/*
    2. Given a list of tuples [(Int,Int)]. Overload the * operator on  [(Int,Int)] such that:
    [(a,b),(c,d)]*[(a2,b2),(c2,d2),(e2,f2)] =
    [(a*a2,b*a2),(a*b2,b*b2),(c*c2,d*c2),(c*d2,d*d2)]
*/


instance * [(Int,Int)]
where
     (*) lst1 lst2 = ( flatten [ [( (fst x) * (fst y), (snd x) * (fst y) ), ( (fst x) * (snd y), (snd x) * (snd y) )] \\ x <- lst1 & y <-lst2 ])

//Start = [(1,2)]*[] //[]
//Start = [(1,2),(3,4),(5,6)]*[(3,5)]//[(3,6),(5,10)]
//Start = [(1,2),(3,4),(5,6)]*[(3,5),(6,2),(4,5),(9,7)]//[(3,6),(5,10),(18,24),(6,8),(20,24),(25,30)]
//Start = [(1,2),(2,1),(3,2)]*[(2,2),(0,0)]//[(2,4),(2,4),(0,0),(0,0)]



/*
    3. Given an array of lists of Ints and an Int, keep the lists whose difference between max and min element is less than the given number
    There are no [] in the array
*/


//minMaxDiff::{​​​​​​​​[Int]}​​​​​​​​ Int->{​​​​​​​​[Int]}​​​​​​​​



//Start = minMaxDiff {​​​​​​​​[1,21,2],[1,1,1,1,1],[1]}​​​​​​​​ 5//{​​​​​​​​[1,1,1,1,1],[1]}​​​​​​​​
//Start = minMaxDiff {​​​​​​​​[1,21],[1..10],[4,3]}​​​​​​​​ 5//{​​​​​​​​[4,3]}​​​​​​​​
//Start = minMaxDiff {​​​​​​​​[1..10],[5..345]}​​​​​​​​ -3//{​​​​​​​​}​​​​​​​​
 

/*
 4. Given array find minimum of it and return new array which has all occurrences of minimum removed.
 For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, minimum is 1, so answer should be {4,5,3,3,2,4,5,3,4}.
*/
 
//rem_min :: {Int} -> {Int}
 
// Start = rem_min {1,4,5,3,3,2,4,5,1,3,4} // {4,5,3,3,2,4,5,3,4}
// Start = rem_min {1,42,42,52,452,4} // {42,42,52,452,4}
// Start = rem_min {5} // {}
// Start = rem_min {} // {}
 
/*
 5. Given two Strings as parameters, remove all characters of second string from the first one.
*/
 
//remove_from_first_string :: String String -> String
 
// Start = remove_from_first_string "Zuka" "z"// "Zuka"
// Start = remove_from_first_string "XccEcxacXmXs aXcrccXe hXaXccXbrXd" "Xbc"// "Exams are hard"
// Start = remove_from_first_string "Clean is the best" " "// "Cleanisthebest"
// Start = remove_from_first_string "It's a nice weather outside" ""// "It's a nice weather outside"
// Start = remove_from_first_string "" ""// ""
 
/*
 6. Given a predefined Shape type, argument of the Circle constructor is the radius,
 side length for Square, and equilateral Triangle, width and height for Rectangle,
 write a function that calculates the area and circumference
 of each shape in the array, store the results of each shape as a tuple in an array.

 
:: Shape = Circle Real
 | Square Real
 | Triangle Real
 | Rectangle Real 
 */
 
//calc :: {Shape} -> {(Real, Real)}
 
//Start = calc {(Circle 3.0), (Square 2.5)} // {(28.26,18.84),(6.25,10)}
// Start = calc {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} // {(8.00640485798713,12.9),(38.88,25.2),(18.84785,15.386)}
// Start = calc {(Triangle 7.6), (Circle 1.75), (Square 0.95)} // {(25.0108136612946,22.8),(9.61625,10.99),(0.9025,3.8)}


/*
 9. Write a filter function for colored rose tree.
 Colored rose Tree is a tree where each node has
 some value, color and children nodes stored in list.
 Your filter function should take tree, color and a
 condition function as an argument. Return a list of
 values stored in nodes which have given color and
 satisfy given condition (Condition function returns
 true for node's value)
*/
 
::NodeColor = Red | Green | Blue
::ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf
 
tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]
 
filterColoredTree :: (ColoredRoseTree a) NodeColor (a -> Bool) -> [a]
 
//Start = filterColoredTree tree1 Blue isEven // [2,4]
// Start = filterColoredTree tree1 Blue isOdd // [3]
// Start = filterColoredTree tree2 Red (\x = True) // [1.7,9,10]
// Start::[Int] // Uncomment this line too, to run next test
// Start = filterColoredTree Leaf Green isOdd // []