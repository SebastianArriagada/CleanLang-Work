module SebastianArriagada_hw7
import StdEnv

/*
	Task 1: 
	
	Update a tree values with the given function.
	
	E.G:
	
	    07
	   /   \
	 02	    20
	 /\	    / \
	01 04  10 30 
	
	TASK: make even values 0 and odd values 1
	
	so result:
	   1
	  / \
	 0	 0
 	/\	 /\
   1  0 0  0 
	   
*/


:: Tree a = Node a (Tree a) (Tree a)
            | Leaf
			
tree1 = Node 7 
						( Node 2 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) 
						( Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))
						
						
tree2 = Node 5 
						( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)) 
						( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))



updateTree :: (Tree Int) -> (Tree Int)
updateTree Leaf = Leaf 
updateTree (Node val tl tr) 
| isEven val = (Node 0 (updateTree tl) (updateTree tr))
= (Node 1 (updateTree tl) (updateTree tr))


//Start = updateTree tree1
			/* 			
						Node 1 
						( Node 0 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) 
						( Node 0 (Node 0 Leaf Leaf) (Node 0 Leaf Leaf)
			*/
			
			
//Start = updateTree tree2
			/* 			
						All are 1s
			*/
			
			
		
			
			

/*
	Given a key determine in which level it is stored in the Tree.
	
	
		 07           <- Level 0
	   /   \          
	 02	    20        <- Level 1
	 /\	    / \ 
	01 04  10 30 	  <- Level 2
*/			

			
getLevel :: Int (Tree Int) -> Int
getLevel num Leaf = 0
getLevel num (Node val tr tl) 
| num == val = 0
|(getLevelAux num tr) < (getLevelAux num tl) = (getLevelAux num tr)
|(getLevelAux num tr) > (getLevelAux num tl) = (getLevelAux num tl)
= (-1)

getLevelAux :: Int (Tree Int) -> Int
getLevelAux num Leaf = 0
getLevelAux num (Node val tr tl) 
| num == val = 0
= 1 + (getLevelAux num tr) + (getLevelAux num tl)


//Start = getLevel 7 tree1 // 0
//Start = getLevel 10 tree1 // 2
//Start = getLevel 55 tree1 // -1
//Start = getLevel 13 tree2 // 2




/*
	Get all possible number that accept division to a given number in the given tree into a list.
	
	
	    07
	   /   \
	 02	    20
	 /\	    / \
	01 04  10 30 
	
	answer: [7] // Since only 7 is divisble by 7.
*/


getDivisors :: Int (Tree Int) -> [Int]
getDivisors num Leaf = []
getDivisors num (Node val tr tl)
| (val rem num) == 0 = [val] ++ getDivisors num tr ++ getDivisors num tl 
= getDivisors num tr ++ getDivisors num tl 


//Start = getDivisors 7 tree1 // [7]
//Start = getDivisors 3 tree2 // [3,9]
//Start = getDivisors 1 tree1 // [7,2,1,4,20,10,30]
//Start = getDivisors 10 tree1 // [20,10,30]


		
			
			
			
			
			
						