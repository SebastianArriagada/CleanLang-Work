module code
import StdEnv

/*--------------------Task-1--------------------*/

/* Complete a record "Entry" that will have
 * 2 fields a string "key" and integer "value".
 * Write a function that takes a array of entries,
 * a key and value. Find this key in the array
 * change it's corresponding value with the new one
 * and return updated entry array. Assume that key is 
 * always in the array.
 */

:: Entry = { key :: String, value :: Int }


set :: String Int {Entry} -> {Entry}
set str num arr = {  setAux x str num  \\ x <-: arr | x.value > 5 }

setAux :: Entry String Int -> Entry 
setAux en str num
| en.key == str = {key = str, value = num}
= {key = en.key, value = en.value}

 
//Start = set "A" 0 {{key="A", value=1},{key="B", value=2}} // {(Entry "A" 0),(Entry "B" 2)}
//Start = set "C" 0 {{key="A", value=1},{key="B", value=2},{key="C", value=2}} // {(Entry "A" 1),(Entry "B" 2),(Entry "C" 0)}
// Start = set "B" 0 {{key="A", value=1},{key="B", value=2},{key="C", value=2}} // {(Entry "A" 1),(Entry "B" 0),(Entry "C" 2)}


/*--------------------Task-2--------------------*/

/* 
The abstract data type LinkedList represents an Imperative-like linked list data Structure.
Complete the function reverse that reverses the linked list

e.g :
  (Node 4 (Node 5(Node 1 Tail)))
    ____\_____________|_
   v                    
= (Node 1 (Node 5(Node 4 Tail)))
*/



:: Prototype a = Node a (Prototype a) | Tail


:: LinkedList a :== (Prototype a)


//reverse :: (LinkedList a) -> (LinkedList a) | Eq a
// TODO

firstList :: (LinkedList Int)
firstList = (Node 4 (Node 5(Node 1 Tail)))

secondList :: (LinkedList Int)
secondList = (Node 2 (Node -6 (Node 45 firstList)))

// Start = reverse firstList // (Node 1 (Node 5 (Node 4 Tail)))
// Start = reverse secondList // (Node 1 (Node 5 (Node 4 (Node 45 (Node -6 (Node 2 Tail))))))

/*--------------------Task-3--------------------*/

:: Month = January | February | March | April | May | June | July | August | September | October | November | December


/* Write a function that takes a list of months and sorts the list by order of the months.
Notably, January should be sorted before February, which should be sorted before March... and so on and so forth.
Duplicates can be kept*/

monthPriority January = 1
monthPriority February = 2
monthPriority March = 3
monthPriority April = 4
monthPriority May = 5
monthPriority June = 6
monthPriority July = 7
monthPriority August = 8
monthPriority September = 9
monthPriority October = 10
monthPriority November = 11
monthPriority December = 12

getMonth 1 = January
getMonth 2 = February
getMonth 3 = March
getMonth 4 = April
getMonth 5 = May
getMonth 6 = June
getMonth 7 = July
getMonth 8 = August
getMonth 9 = September
getMonth 10 = October
getMonth 11 = November
getMonth 12 = December


monthSort :: [Month] -> [Month]
monthSort [] = []
monthSort mon = map getMonth(sort(map monthPriority mon))




Start = monthSort [February, October, January, June, December, May, April, October] // [January,February,April,May,June,October,October,December]
//Start = monthSort [January, January, October, June, December, May, April, October] // [January,January,April,May,June,October,October,December]
//Start = monthSort [] //[]