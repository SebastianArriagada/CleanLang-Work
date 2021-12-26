module pt7gr6

import StdEnv 

/* 
 * Create a student record with 3 fields: name - String, id - String, grades - Array of integers.
 * Write a function LetterGrades that takes array of students, and returns a list of tuples (name,the grades in Letter system).
 * a grade is A if it above 90
 * a grade is B if it above 80
 * a grade is C if it above 70
 * a grade is D if it above 60
 * a grade is E if it 50 or above
 * a grade is F if it below 50
 * 
 */

:: Student = {name :: String
             , id :: String
             , grades :: {Int}
             }


LetterGrades :: {Student} -> [(String,[Char])]
LetterGrades array = [( array.name , ) ]

// Intended for tests. Do not remove!
student1 = {name="a",id="st1",grades={20,40,13,50, 70, 80, 90}}
student2 = {name="b",id="st2",grades={50,13,10,90}}
student3 = {name="c",id="st3",grades={13,60}}
student4 = {name="d",id="st4",grades={}}

//Start = LetterGrades {} // []
Start = LetterGrades {student1} // [("a",['F','F','F','E','D','C','B'])]
//Start = LetterGrades {student1, student2, student3, student4} // [("a",['F','F','F','E','D','C','B']),("b",['E','F','F','B']),("c",['F','E']),("d",[])]
//Start = LetterGrades {student4} // [("d",[])]