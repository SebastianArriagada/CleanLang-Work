module SebastianArriagada_hw9
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 1 (Node 2 (Node 4 Leaf Leaf)(Node 5 Leaf Leaf)) (Node 1 (Node 6 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 4 (Node 3 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 0 (Node 2 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 1 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node -1 (Node -3 (Node -11 (Node -2 Leaf Leaf) Leaf) Leaf) (Node -16 (Node -22 (Node -13 (Node -15 Leaf Leaf) Leaf) Leaf) (Node -100 Leaf (Node -1 Leaf Leaf)) ))


/*
1. Given a Tree of integers, return a list of nodes' value where sum of its' children is equal to 3.
*/


getNodes :: (Tree Int) -> [Int]
getNodes Leaf = []
getNodes ( Node val tr tl )
| getChilds ( Node val tr tl ) == 3 = [val] ++ getNodes tr ++ getNodes tl 
= getNodes tr ++ getNodes tl 

getValue :: (Tree Int) -> Int
getValue Leaf = 0
getValue (Node val tr tl) = val

getChilds :: (Tree Int) -> Int
getChilds Leaf = 0
getChilds (Node val tr tl) = getValue tr + getValue tl

//Start = getNodes tree1 // [1]
//Start = getNodes tree2 // [4,0]
//Start = getNodes tree3 // []

:: Major = CS | Math | Physics | Linguistics
:: Course = {name::String, major :: Major, credits:: Int}

Programming::Course
Programming = {name="Programming",major=CS, credits =5}
Analysis::Course
Analysis = {name="Analysis",major=Math, credits =4}
Relativity::Course
Relativity = {name="Relativity",major=Physics,credits=5}
Functional::Course
Functional = {name="Functional",major=CS,credits=5}
BasicMath::Course
BasicMath = {name="BasicMath",major=Math,credits=2}
ThermoDynamics::Course
ThermoDynamics = {name="ThermoDynamics",major=Physics,credits=4}
Astronomy::Course
Astronomy = {name="Astronomy",major=Physics,credits=6}
Compilers::Course
Compilers = {name="Compilers",major=CS,credits=4}
Hungarian::Course
Hungarian = {name="Hungarian",major=Linguistics,credits=3}
English::Course
English = {name="English",major=Linguistics,credits=3}

/*
2. Given a list of Courses that a student has taken,
return the credits he/she earned for elective and mandatory course. 
CS major courses are mandatory for the student and others should be counted as elective courses.
*/

creds :: [Course] -> (Int,Int)
creds [] = (0,0)
creds data = ( sum[ x.credits \\ x <- data | isMandatory (x.major) ] , sum[ x.credits \\ x <- data | notMandatory (x.major) ]  )

notMandatory CS =  False
notMandatory _ = True

isMandatory CS = True
isMandatory _ = False 

//Start = creds [Compilers, Astronomy, ThermoDynamics] // (4,10)
//Start = creds [English, Relativity, Programming, Functional] // (10,8)
//Start = creds [Compilers, Hungarian, BasicMath, Analysis, Programming] //(9,9)
//Start = creds [] // (0,0)


/* 3.
Define an instance of the built-in class ==
for Course. Courses are equal if they have same
major and the difference between credits is less than 2.
*/

instance == Course
where 
     (==) as1 as2 = (abs(as1.credits - as2.credits)) < 2

//Start = English == Hungarian // True
//Start = BasicMath == Analysis // False
//Start = Programming == Functional // True
//Start = Astronomy == Compilers // False
