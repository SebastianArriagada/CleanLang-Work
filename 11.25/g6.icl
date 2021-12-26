module g6
import StdEnv

:: LinkedList a = Node a (LinkedList a) | Tail

/*
Write a function that takes a LinkedList and N as an argument
and returns a linked list that contains first N elements. If
N is greater than the number of elements whole initial list is returned.
Example:
    Node 4 (Node 1 Tail) 0 -> Tail
    Node 4 (Node 1 Tail) 1 -> Node 4 Tail
    Node 4 (Node 1 Tail) 2 -> Node 4 (Node 1 Tail)
    Node 4 (Node 1 Tail) 3 -> Node 4 (Node 1 Tail)
    and so on.
*/

linkedListTake :: (LinkedList a) Int -> (LinkedList a)
linkedListTake Tail _ = Tail
linkedListTake (Node val tl) num 
| (howMenyLeaft (Node val tl)) == num = tl
= linkedListTake tl num

howMenyLeaft :: (LinkedList a)  -> Int 
howMenyLeaft Tail = 1
howMenyLeaft (Node val tl)  = 1 + (howMenyLeaft tl)

//Start = howMenyLeaft (Node 4 (Node 1 Tail)) 

//Start = linkedListTake (Node 4 (Node 1 Tail)) 0 // Tail
//Start = linkedListTake (Node 4 (Node 1 Tail)) 1 // Node 4 Tail
// Start = linkedListTake (Node 4 (Node 1 Tail)) 2 // Node 4 (Node 1 Tail)
// Start = linkedListTake (Node 4 (Node 1 Tail)) 3 // Node 4 (Node 1 Tail)
Start = map (foldr (*) 1) [[1,2,3], [4,5]] 