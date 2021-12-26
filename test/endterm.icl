module endterm

import StdEnv

// Please fill the data required below.
//<Sebastian Ignacio Arriagada Silva> 
//<HSO14Z>
//Functional Programming & end-term
//2021. December.13 
//This solution was submitted and prepared by <Sebastian Ignacio Arriagada Silva, HSO14Z> 
//for the end-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students� regulation of E�tv�s Lor�nd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student�s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.



/**** BEST OF LUCK!! ****/

/* 

	Task 1: 
	Given an array of arrays (We will denote the inner arrays as rows), and
	another array (we will denote it by "Key"). Your task is to encrypt
	each of the given rows with the given Key array. The encryption can be simple,
	you only need to add the "key" sequence values to each of the rows. If the row
	is larger than the "key" then the key keeps repeating.
	Assume that there are no empty arrays as inputs.
	And Rows and keys can be of any length (rows: {{},{},{}...}, keys: {......}).
	
	
	e.g:
	rows:
	{
		{1,2,3,4,5},
	 	{5,4,3,2,1} 
	}
	
	key: {1,2,3}
	
	result:
	{
		{1+1,2+2,3+3,4+1,5+2},
		{5+1,4+2,3+3,2+1,1+2}
	}
	or in other words:
	{
		{2,4,6,5,7},
		{6,6,6,3,3}
	}
	The above matrix is the encrypted matrix.
*/


//encryptMat :: {Int} {{Int}} -> {{Int}}
//encryptMat key rows = { {x+z} \\ x<-:key , z <-eachRow }


//Start = encryptMat {1,2,3} {{1,2,3,4,5}, {5,4,3,2,1}}			// {{2,4,6,5,7},{6,6,6,3,3}}
//Start = encryptMat {1} {{1,0,0,0,0}, {5,4,3,2,1}} 			// {{2,1,1,1,1},{6,5,4,3,2}}
//Start = encryptMat {-1,-2,-3} {{2,4,6,5,7},{6,6,6,3,3}}		// {{1,2,3,4,5}, {5,4,3,2,1}}
//Start = encryptMat {1,2,3} {{1,0}} 								// {{2,2}}


// --------------------------


/*
	Task 2:
		Given an array, return an array that holds 
		tuples of the unique elements and their occurrences. 
		In other words, construct a bag from the given array.
*/


arrayBag :: {Int} -> {(Int,Int)}
arrayBag arr = { (x, (countArr x arr)) \\ x <- (uniquesArr arr) }

uniquesArr :: {Int} -> [Int]
uniquesArr arr = removeDup[ z \\ z<-:arr]

countArr :: Int {Int}  -> Int 
countArr num arr = sum[1 \\ x<-:arr | x == num]


//Start = arrayBag {}								// {}
//Start = arrayBag {1,1,1,3,3,3,2,4,5} 				// {(1,3),(3,3),(2,1),(4,1),(5,1)}
//Start = arrayBag {1,1,1,3,3,3,2,4,5,1,1,4,4,5,5} 	// {(1,5),(3,3),(2,1),(4,3),(5,3)}
//Start = arrayBag {1,2,3,4,5}				 		// {(1,1),(2,1),(3,1),(4,1),(5,1)}
//Start = arrayBag {5,5,5,5,5}						// {(5,5)}


//----------------


:: Degree =   BSC | MSC | PHD     
:: Company = {companyName :: String, numberOfEmloeeys :: Int, employees :: {Employee}} 
:: Employee = { name :: String,
                salary :: {Real}, //  array containing the salary of an employee for 12 months 
                age :: Int, 
                degree :: Degree   
              }

e1 :: Employee
e1 = {name = "e1" , salary = {10.0,20.0,30.0,40.0,50.0,10.0,20.0,30.0 ,40.0,50.0 ,10.0,90.0} , age = 40 , degree = BSC}
e2 :: Employee
e2 = {name = "e2" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 30 , degree = BSC}
e3 :: Employee
e3 = {name = "e3" , salary = {50.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0 ,20.0,20.0 ,20.0,20.0} , age = 60 , degree = MSC}
e4 :: Employee
e4 = {name = "e4" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 45 , degree = MSC}
e5 :: Employee
e5 = {name = "e5" , salary = {100.0,110.0,110.0,40.0,50.0,30.0,30.0,30.0 ,140.0,50.0} , age = 80 , degree = PHD}
e6 :: Employee
e6 = {name = "e6" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 45 , degree = PHD}

cm1 :: Company
cm1 = {companyName = "cm1", numberOfEmloeeys = 3, employees = {e1,e3,e5}}
cm2 :: Company
cm2 = {companyName = "cm2", numberOfEmloeeys = 2, employees = {e1,e2}}
cm3 :: Company
cm3 = {companyName = "cm3", numberOfEmloeeys = 2, employees = {e1,e3}}
cm4 :: Company
cm4 = {companyName = "cm4", numberOfEmloeeys = 3, employees = {e1,e3,e5}}
cm5 :: Company
cm5 = {companyName = "cm5", numberOfEmloeeys = 3, employees = {e2,e3,e6}}


/*
	Task 3:
		 Given a list of Companies, return the companyNames of all the good companies in the list,
		 a company is considered to be good if the annual salary of every employee is at least:
				100.0 BSC holder,
				200.50 MSC holder,
				and 500.64 PHD holder. 
*/

isBSC BSC = True 
isBSC _ = False

isMSC MSC = True 
isMSC _ = False 

isPHD PHD = True 
isPHD _ = False

goodCompanies :: [Company] -> [String]
goodCompanies companyList = [ (company.companyName) \\ company <- companyList | (100.0 <= sumReal [ (sumArray worker.salary ) \\ worker <-: company.employees | isBSC (worker.degree)])	 && (200.5 <= sumReal [ (sumArray worker.salary ) \\ worker <-: company.employees | isMSC (worker.degree)]	)  && (500.64 <= sumReal [ (sumArray worker.salary ) \\ worker <-: company.employees | isPHD (worker.degree)])	  ]

sumReal :: [Real] -> Real 
sumReal [] = 0.0 
sumReal list = (hd list) + (sumReal (tl list))  
	
sumArray :: {Real} -> Real
sumArray arr = sum[z \\ z<-: arr]

//Start = goodCompanies [cm1,cm2,cm3]  // ["cm1","cm3"]
//Start = goodCompanies [cm1,cm2,cm3,cm4,cm5] // ["cm1","cm3","cm4"]
//Start = goodCompanies [cm2,cm5] // []
//Start = goodCompanies [cm1,cm3,cm4] // ["cm1","cm3","cm4"]


//-----------------


:: TruckType = TOYOTA | BMW | KIA
:: Truck = {name::String, type :: TruckType, reputation:: [Int]}

toyota1::Truck
toyota1 = {name="toyota1",type=TOYOTA, reputation =[5,5,3,4,2,4,5,5]} // toyota1
toyota2::Truck
toyota2 = {name="toyota2",type=TOYOTA, reputation=[5,5,5,5,3,4,5,4,5]} // toyota2
toyota3::Truck
toyota3 = {name="toyota3",type=TOYOTA, reputation=[2,3,3,4,3,2,1,3,2,3]} // toyota3

bmw1::Truck
bmw1 = {name="bmw1",type=BMW, reputation =[3,2,3,4,2,4,2,1,4,3,2,4]} // bmw1
bmw2::Truck
bmw2 = {name="bmw2",type=BMW, reputation=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]} // bmw2
bmw3::Truck
bmw3 = {name="bmw3",type=BMW, reputation=[5,5,5,4,5,5,4,5,4,5]} // bmw3

kia1::Truck
kia1 = {name="kia1",type=KIA, reputation=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]} // kia1
kia2::Truck
kia2 = {name="kia2",type=KIA, reputation=[3,4,1,3,4,2,3,5,5]} // kia2
kia3::Truck
kia3 = {name="kia3",type=KIA, reputation=[4,2,4,4,4,4,4,5,2]} // kia3


/*
	Task 4: 
		Given an array of Trucks, find the TruckType (Toyota, Kia, BMW) having the
		smallest average of all its cars' reputation averages. 
		
Example:
	{bmw1, kia3, kia2}
	bmw1's safety reputation average - 2.83
	kia3's safety reputation average - 3.67
	kia2's safety reputation average - 3.33
 
	Hence:
		BMW's average reputation - [2.83]
		KIA's average reputation - [3.67, 3.33]
		
		We see that the BMWs average (2.83) is smaller than KIA's average ((3.67 + 3.33) / 2)
		
		BMW has smallest average - 2.83 in the given array {bmw1, kia3, kia2}
*/

isBMW BMW = True 
isBMW _ = False

isKIA KIA = True
isKIA _ = False 

isTOYOTA TOYOTA = True 
isTOYOTA _ = False

getTruck :: {Truck} -> TruckType
getTruck trucks 
| bmw == 0.0 = getTruckAux 1000.0 kia toyota
| kia == 0.0 = getTruckAux bmw 1000.0 toyota
| toyota == 0.0 = getTruckAux bmw kia 1000.0
| bmw < kia && bmw < toyota = BMW 
| kia < bmw && kia < toyota = KIA 
=TOYOTA
where
	 bmw = avg ([ realAvg (x.reputation) \\ x<-:trucks | isBMW (x.type) ] ++ [0.0])
	 kia = avg ([ realAvg (x.reputation) \\ x<-:trucks | isKIA (x.type) ]++ [0.0])
	 toyota = avg ([ realAvg (x.reputation) \\ x<-:trucks | isTOYOTA (x.type) ]++ [0.0])

getTruckAux :: Real Real Real -> TruckType
getTruckAux  bmw kia toyota
| bmw == 0.0 = getTruckAux 1000.0 kia toyota
| kia == 0.0 = getTruckAux bmw 1000.0 toyota
| toyota == 0.0 = getTruckAux bmw kia 1000.0
| bmw < kia && bmw < toyota = BMW 
| kia < bmw && kia < toyota = KIA 
=TOYOTA

realAvg :: [Int] -> Real 
realAvg [] = 0.0
realAvg arr = avg[ toReal x \\ x<-arr ]

//Start = getTruck {toyota1, kia2, bmw3, toyota3, toyota2, kia1, bmw2, bmw1, kia3} // KIA
//Start = getTruck {toyota1, kia2, bmw3} // KIA
//Start = getTruck {toyota3, kia1, toyota2, bmw2} // BMW
//Start = getTruck {bmw1, kia3, kia2} // BMW


//-----------------


:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

/* Task 5: 
	BE AWARE THAT THIS TASK CONSISTS OF MULTIPLE SMALL TASKS. THUS, IT IS WORTH DOUBLE POINTS (20 POINTS).

	The type above (Day) represents a day in the week
	
	TASKS:
	a) Complete the function isWorkDay that checks if the day is 
		a work day.
	
	b) Complete the function "whichDay" that checks what day was on the 
		parameter date. Assuming that every month has 30 days and that every
		month starts with a Monday.
	
	c) "advancedWhichDay" is a function that takes an integer date and a 
		month, and checks what day of the month it was on that date.
		Assuming that January starts with a Monday, 
		and that every month has 30 days.
		
	d) "mondayLister is a function that takes a year, and a month,
		and returns dates of the Mondays of that year's month.
		Assuming that "1960 January 1st" is a Monday,
		and that every month has 30 days. Moreover, assuming there are no days before 
		"1960 January 1st".
	
*/

//isWorkDay :: Day -> Bool

//whichDay :: Int -> Day

//advancedWhichDay :: Month Int -> Day 

//mondayLister :: Int Month -> [Int]


// Start = isWorkDay Mon // True
// Start = isWorkDay Friday // True
// Start = isWorkDay Sunday // False

// Start = whichDay 1 // Mon
// Start = whichDay 27 // Sat
// Start = whichDay 0 // "Day does not exist"
// Start = whichDay 35 // "Day does not exist"

// Start = advancedWhichDay Jan 1 // Mon
// Start = advancedWhichDay Feb 28 // Tue
// Start = advancedWhichDay Dec 9 // Wed
// Start = advancedWhichDay Oct 29 // Fri

// Start = mondayLister 1960 Jan // [1,8,15,22,29]
// Start = mondayLister 2021 Jan // [7,14,21,28]
// Start = mondayLister 1963 Sep // [4,11,18,25]
// Start = mondayLister 1989 Nov // [6,13,20,27]


//-----------------


/* Task 6
	Given the rose tree and depth of the level we want to delete,
	remove all elements at that level. Children nodes of the deleted
	node should become children of its parent (hence push up by 1 level).
	For example, if we have a tree:
                      1                               Level: 0
                   /     \
                  2       3                           Level: 1
               /  |  \    |  \                   
              4   5  6    7   8    <-- Row to delete. Level: 2
            / | \   / \   | \  \ 
           9 10 11 12 13 14 15 16                     Level: 3

	After deleting 2nd level row, nodes (4,5,6,7 and 8) are deleted
	and their children (9,10,11,12,13,14,15 and 16) become children of
	the 1st level row (2 and 3). Hence we get a new tree:
                      1                               Level: 0
                  /       \
                 2          3                         Level: 1
            / |  |  | \   / | \
           9 10 11 12 13 14 15 16                     Level: 2

	NOTE: Levels start from 0 not 1. You are guaranteed that you are
	never asked to delete the first row (Level 0).
*/

:: RoseTree a = TNode a [(RoseTree a)]

tree1 = TNode 5 []
tree2 = TNode 3 [(TNode 2 [(TNode 3 [])]), tree1, tree1]
tree3 = TNode 1 [tree1, tree2, tree2]


//deleteRow :: Int (RoseTree a) -> (RoseTree a) 


// Start = deleteRow 5 tree1 // (TNode 5 [])
// Start = deleteRow 1 tree2 // (TNode 3 [(TNode 3 [])])
// Start = deleteRow 1 tree3 // (TNode 1 [(TNode 2 [(TNode 3 [])]),(TNode 5 []),(TNode 5 []),(TNode 2 [(TNode 3 [])]),(TNode 5 []),(TNode 5 [])])
// Start = deleteRow 2 tree3 // (TNode 1 [(TNode 5 []),(TNode 3 [(TNode 3 [])]),(TNode 3 [(TNode 3 [])])])


//-----------------


:: FlexTree a = TernaryNode a (FlexTree a) (FlexTree a) (FlexTree a)
              | BinaryNode a (FlexTree a) (FlexTree a)
              | UnaryNode a (FlexTree a)
              | TerminalNode

ftree1 = UnaryNode 1 (BinaryNode 2 TerminalNode TerminalNode)
ftree2 = BinaryNode 1 TerminalNode ftree1
ftree3 = TernaryNode 3 TerminalNode (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode)
ftree4 = TernaryNode 1 ftree2 TerminalNode (BinaryNode 2 (TernaryNode 1 TerminalNode TerminalNode TerminalNode) (BinaryNode 2 ftree2 ftree3))


/* 
	Task 7:
		Flex Tree can have 4 types of nodes: Ternary, Binary, Unary and Terminal. As
		names suggest these nodes have 3, 2, 1 and 0 children nodes respectively. 
		Terminal nodes do not store any value, they indicate end of the tree.

		Write a function that takes a flex tree as an argument and returns pruned 
		flex tree back. Pruning a tree means changing its node types based on the
		unused children. For example: If you have a ternary node that has exactly 1
		TerminalNode, it should be deprecated to binary node, as having a terminal node
		does not make sense and it is waste of space. If ternary node has exactly 2 terminal
		nodes it should be deprecated to UnaryNode. If ternary node has exactly 3 terminal
		nodes than it is still deprecated to UnaryNode (we can't change it to terminal because 
		the value stored in the ternary node should be preserved) which will have 1 terminal 
		node as a child. Same rules apply to binary node as well, if it has 1 or 2 
		terminal nodes, it should be deprecated to UnaryNode.

		Your function should return a new FlexTree after all unnecessary  TerminalNodes are
		removed and nodes are deprecated as needed. Hence the tree is pruned.

For example, if we have a FlexTree:

                 (TernaryNode 1)
            /          |         \
    (BinaryNode 2) TerminalNode (UnaryNode 3)
    /          \                       |            
TerminalNode (UnaryNode 4)        (BinaryNode 5)
                  |                 /           \
             TerminalNode       TerminalNode  TerminalNode


After pruning it should look like this:
                 (BinaryNode 1)
                /               \
        (UnaryNode 2)        (UnaryNode 3)
             |                    |            
        (UnaryNode 4)        (BinaryNode 5)
             |                    |           
        TerminalNode          TerminalNode  
*/


//pruneTree :: (FlexTree a) -> (FlexTree a)



// Start = pruneTree TerminalNode // TerminalNode
// Start = pruneTree ftree1 // (UnaryNode 1 (UnaryNode 2 TerminalNode))
// Start = pruneTree ftree2 // (UnaryNode 1 (UnaryNode 1 (UnaryNode 2 TerminalNode)))
// Start = pruneTree ftree3 // (BinaryNode 3 (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode))
// Start = pruneTree ftree4 // (BinaryNode 1 (UnaryNode 1 (UnaryNode 1 (UnaryNode 2 TerminalNode))) (BinaryNode 2 (UnaryNode 1 TerminalNode) (BinaryNode 2 (UnaryNode 1 (UnaryNode 1 (UnaryNode 2 TerminalNode))) (BinaryNode 3 (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode)))))


//----------------------


:: Major = CS | Math | Physics | Linguistics
:: Course = {name::String, major :: Major, credits:: Int, max_student:: Int}

Programming::Course
Programming = {name="Programming",major=CS, credits =5, max_student=50}

Functional::Course
Functional = {name="Functional",major=CS,credits=5, max_student=120}

Imperative::Course
Imperative = {name="Imperative",major=CS,credits=4, max_student=150}

Analysis::Course
Analysis = {name="Analysis",major=Math, credits =4, max_student=85}

BasicMath::Course
BasicMath = {name="BasicMath",major=Math,credits=2, max_student=40}

Hungarian::Course
Hungarian = {name="Hungarian",major=Linguistics,credits=3, max_student=20}


/* 
	Task 8:
		Create an instance `+` for the record Course, 
		such that for two records C1 and C2 when added (C1+C2) will give
 		a record Course where:
 			- name is the concatenation of the first letter in C1's and C2's names.
 			- major is the course with the greatest max_student.
 			- credits is the greatest credit if courses have same major,
 				or the average of credits, if the courses have different major (you don't have to use real number for average)
 			- max_student is C1's max_student plus C2's max_student.


*/
instance + Course
where
	 (+) curse1 curse2 =   { name = "I always got the error: name ambiguous selector specified :(", major = (getHigherMajor curse1 curse2), credits = (getGrade curse1 curse2 ), max_student = (curse1.max_student + curse2.max_student) }

sameMajor CS CS = True
sameMajor Math Math = True 
sameMajor Physics Physics = True 
sameMajor Linguistics Linguistics = True
sameMajor _ _ = False

//getname :: Course Course -> String 
//getname curse1 curse2 = (toString ( select (curse1.name) 0) ) +++ (toString ( select ( curse2.name) 0 ))

getHigherMajor :: Course Course -> Major 
getHigherMajor curse1 curse2
| curse1.max_student < curse2.max_student = curse2.major
= curse1.major

getGrade :: Course Course -> Int 
getGrade curse1 curse2
| (sameMajor (curse1.major)  (curse2.major)) = greatGrade curse1 curse2
= ( curse1.credits + curse2.credits) /2

greatGrade :: Course Course -> Int 
greatGrade curse1 curse2 
|curse1.credits < curse2.credits =  curse2.credits 
=  curse1.credits 


//Start = Functional + Imperative // (Course "FI" CS 5 270)
//Start = BasicMath + Hungarian // (Course "BH" Math 2 60)
//Start = Analysis + Programming // (Course "AP" Math 4 135)
//Start = Imperative + BasicMath // (Course "IB" CS 3 190)


//-----------------------


/* Task 9:
	Create a class named Comparisons and define the binary operations:
		*== , != , *< , *>, *>>
	It compares given two elements and returns a Boolean.
	Make an instance for Course type. To compare two courses, follow the example below.
	Explanation: 
		*==  ->  checks if the courses have same major and the difference between max_students is less than 50
		*<>  ->  checks if the courses have different major and the difference between max_students is greater than 50
		*<   ->  compares the (courses credits plus name length)
		*>   ->  compares the (courses credits plus name length)
		*>>  ->  returns the greater value of (the courses credits plus name length)
*/


//class Comparisons ...


//instance ...


//Start = Programming *== Functional // False
//Start = Imperative *== Analysis // False
//Start = Programming *<> Imperative // False
//Start = Hungarian *> BasicMath // True
//Start = Programming *< Analysis // False
//Start = Hungarian *>> Analysis // 12
//Start = Analysis *>> Hungarian // 12


//----------------------


/*
	Task 10:
		Given an array of names, return the name that contains the most 
		repetitive letters.
		
	repetitive letters is the number of repetitions for each unique letter in the name
	
	Example:
	mohammed has 1 repetitive letter (m)
	abdullah has 2 repetitive letters (a,l)
	Therefore, "Abdullah" is complex name, thus we return it.
*/


//mostComplexName :: {String} -> String


//Start = mostComplexName {"mohammed","abdullah"}		 // "abdullah"
//Start = mostComplexName {"mohammed","beka"} 			// "mohammed"
//Start = mostComplexName {"beka","tariq"} 				// "beka" //Both are with same complexity, so we return first one
//Start = mostComplexName {"beka", "mohammed", "abdullah", "tariq", "hello"}		//"abdullah"
//Start = mostComplexName {"beka", "tariq", "hello"}	// "hello"


//---------------


/*
	Task 11:
		Given 2 inputs (an array and a number denoted by "nLen"). 
		return the maximum contiguous subarray of size "nLen" having the maximum sum.
		E.g:
		 	{1, 2, -2, -1,-5, 10, 4} 2    ->    {10, 4}
		 	 \+/ \+/ \+/ \+/\+/\+/
		 	  3   0  -3  -6  5  14
		 	Explanation:
		 	We went through all the possible contiguous subarrays in the array and got the subarray 
		 	with the maximum sum with length 2.
		 	
	Preconditions:
		1) nLen is smaller than the given array size
		2) Given array size is always larger than 2
		
*/


maximumSubarray :: {Int} Int -> {Int}
maximumSubarray arr num =  { y \\ y<- snd (splitAt (length [ x \\ x<-:arr ] - num) (sort [ x \\ x<-:arr ]))}

//Start = maximumSubarray {1, 2, -2, -1, -5, 10, 4} 2			// {10,4}
//Start = maximumSubarray {1, 2, 3 , 10} 3 						// {2,3,10}
//Start = maximumSubarray {1, 2, 3 , -10} 3 					// {1,2,3}


//---------------