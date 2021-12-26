module pt10gr6

import StdEnv


/*
A priority queue is an abstract data structure that holds elements similar to a queue. 
Each element additionally has a "priority" associated with it.
The priority queue is Always sorted in ascending order according to the priority.

Given the abstract data type priority queue, implement the function insert that inserts
the given element to the last position in the queue while considering its new priority.
*/

// :: PriorityQueue a :== [(Int,a)]



//insert :: (PriorityQueue a) a -> (PriorityQueue a)

Start = removeIndex 5 [1, 2, 5, 8]
// Start = insert [(1,2), (2,582), (15, 8996)] 52114 // [(1,2),(2,582),(15,8996),(16,52114)]
// Start = insert [] 50 // [(1,50)]
// Start = insert [(23,True), (56,False), (89, False)] True // [(23,True),(56,False),(89,False),(90,True)]

