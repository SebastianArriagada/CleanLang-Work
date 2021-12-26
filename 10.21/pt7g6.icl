module pt7g6
import StdEnv

/* Given the list of tuples, where each tuple
 * contains name and the points of the student.
 * Write a function that will anotate each data
 * with final mark. Each tuple should be changed
 * with a new tuple, which in addition to name
 * and grade will contain the final mark. The mark
 * is calculated based on the following rules:
 * 5 - If point >= 85.
 * 4 - If 85 > point >= 70
 * 3 - If 70 > point >= 55
 * 2 - If 55 > point >= 45
 * 1 - Otherwise.
 */
task :: [(String, Int)] -> [(String, Int, Int)]
task listOfTuples 
| listOfTuples == [] = []
= [ ( fistElement, secondElement, getGrade (secondElement) )] ++ task (tl listOfTuples )
where 
        fistElement = fst (hd listOfTuples )
        secondElement = snd (hd listOfTuples )

getGrade :: Int -> Int 
getGrade num
| num >= 85 = 5
| num < 85 && num >= 70 = 4 
| num < 70 && num >= 55 = 3
| num < 55 && num >= 45 = 2
= 1


Start = task [("A",91),("B", 36),("C",78)] // [("A",91,5),("B",36,1),("C",78,4)]
//Start = task [("A",91),("B", 35),("C",78),("D",12),("E",34)] // [("A",91,5),("B",35,1),("C",78,4),("D",12,1),("E",34,1)]
//Start = task [] // []
// Start = task [("A",100),("B", 85),("C",78)] // [("A",100,5),("B",85,5),("C",78,4)]
//Start = task [("A",12)] // [("A",12,1)]