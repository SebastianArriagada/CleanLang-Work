module SebastianArriagada_hw10
import StdEnv 


:: University = ELTE | BME | Corvinus
:: Student = {name::String, uni :: University, grades:: [Int]}
 

Rose::Student
Rose = {name="Rose",uni=ELTE, grades =[5,5,3,4,2,4,5,5]}
Peter::Student
Peter = {name="Peter",uni=BME, grades =[3,2,3,4,2,4,2,1,4,3,2,4]}
Noah::Student
Noah = {name="Noah",uni=Corvinus,grades=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]}
James::Student
James = {name="James",uni=ELTE,grades=[5,5,5,5,3,4,5,4,5]}
Lily::Student
Lily = {name="Lily",uni=BME,grades=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}
Harry::Student
Harry = {name="Harry",uni=Corvinus,grades=[3,4,1,3,4,2,3,5,5]}
Eros::Student
Eros = {name="Eros",uni=Corvinus,grades=[4,2,4,4,4,4,4,5,2]}
Isabella::Student
Isabella = {name="Isabella",uni=BME,grades=[5,5,5,4,5,5,4,5,4,5]}
Oliver::Student
Oliver = {name="Oliver",uni=ELTE,grades=[2,3,3,4,3,2,1,3,2,3]}
 

/* 1.
Given array of students, find the University which has highest
average of student average GPA.
 

Example:
{Peter, Eros, Harry}
Peter's average GPA - 2.83
Eros's average GPA - 3.67
Harry's average GPA - 3.33
 

Hence:
ELTE's average grades - []
BME's average grades - [2.83]
Corvinus's average grades - [3.67, 3.33]
 

Corvinus has highes average - 3.5
*/
 



uniWithHighestAverage :: {Student} -> University
uniWithHighestAverage arr 
| elte >= bme && elte >= corvinus = ELTE
| bme >= elte && bme >= corvinus = BME
= Corvinus 

where
     elte = avg([ avg( realList (x.grades) )\\ x <-: arr | isElte (x.uni) ] ++ [0.0])
     bme = avg([ avg(realList (x.grades)) \\ x <-: arr | isBme (x.uni) ] ++ [0.0])
     corvinus = avg([ avg(realList (x.grades)) \\ x <-: arr | isCorvinus (x.uni) ] ++ [0.0])

realList :: [Int] -> [Real]
realList [] = []
realList lst = [toReal x \\ x <- lst]

isElte ELTE = True 
isElte _ = False 

isBme BME = True 
isBme _ = False 

isCorvinus Corvinus = True
isCorvinus _ = False 


//Start = uniWithHighestAverage {Rose,Harry,Isabella,Oliver,James,Noah,Lily,Peter,Eros} // ELTE
//Start = uniWithHighestAverage {Rose,Harry,Isabella} // BME
//Start = uniWithHighestAverage {Oliver, Noah,James,Lily} // ELTE
//Start = uniWithHighestAverage {Peter, Eros, Harry} // Corvinus


/*
2- Implement a function that interleaves three arrays. So for input arrays {1,2,3}, {4,5,6}
and {7,8,9} the function must return the array {1,4,7,2,5,8,3,6,9}. If an array is out of elements
we continue interleaving the remaining arrays.
Example: {1,2} {3,4,5,6} {7,8,9} -> {1,3,7,2,4,8,5,9,6}


interleave :: {Int} {Int} {Int} -> [Int]
//rleave {} {} {} = {}
interleave arr1 arr2 arr3 =  removeDup(flatten[[x , y , z] \\ x <-: arr1, y <-: arr2,  z <-: arr3 ])
*/
//Start = interleave {1,2,3} {4,5,6} {7,8,9} // {1,4,7,2,5,8,3,6,9}
// Start = interleave {1,2} {3,4,5,6} {7,8,9} // {1,3,7,2,4,8,5,9,6}
// Start = interleave {} {1,2,3} {4} // {1,4,2,3}
// Start = interleave {} {} {} // {}
// Start = interleave {1,2} {3,4,5} {6,7,8,9,10,11,12} // {1,3,6,2,4,7,5,8,9,10,11,12}



/*
3- Given a predefined MaybeInt type, define a new operator !+!
for accessing the nth element in the list, you can test it with showFifthElement function.


:: MaybeInt = Just Int | Nothing

(!+!) :: [Int] Int -> MaybeInt

showFifthElement :: [Int] -> String
showFifthElement xs
= case xs !+! 4 of
Nothing -> "There is no fifth element in this list"
Just n -> "The fifth element of the list is: " +++ toString(n)
*/
// Start = showFifthElement [1,2..10] // "The fifth element of the list is: 5"
// Start = showFifthElement [0,0] // "There is no fifth element in this list"
// Start = showFifthElement [33, 41, 56, 12, 96, 1] // "The fifth element of the list is: 96"



Start = 33.0/8.0

