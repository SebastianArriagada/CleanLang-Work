module Sebastian_Arriagada_hw8
import StdEnv



:: Tree a = Node a (Tree a) (Tree a) | Leaf


// Trees for testing, please do not remove them  . 
tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))

tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))

tree5 :: Tree Int
tree5 = Node 1 tree3 tree4





/* 1. Given the binary tree, find how many nodes are there such that they have exactly
3 grandchildren non-leaf nodes.
Ex.:  1
    /   \
    2    3
   / \  / \
  4 5   6 Leaf
1st node has exactly 3 grandchildrens, so it's a 'good' node.

*/

countTripleParents :: (Tree Int) -> Int
countTripleParents Leaf = 0
countTripleParents (tree) = (countTripleParentsAux tree)/2

countTripleParentsAux :: (Tree Int) -> Int
countTripleParentsAux Leaf = 0
countTripleParentsAux (Node val tr tl) 
| (countParents (Node val tr tl) ) == 3 = 1 + (countTripleParentsAux tr) + (countTripleParentsAux tl)
= (countTripleParentsAux tr) + (countTripleParentsAux tl)

countParents :: (Tree Int) -> Int
countParents Leaf = 1
countParents (Node val tr tl) = 1 + (((countParents tr) +  (countParents tl))/ 2  )


//Start = countTripleParents tree1 // 1
//Start = countTripleParents tree2 // 1
//Start = countTripleParents tree3 // 1
//Start = countTripleParents tree4 // 4
//Start = countTripleParents tree5 // 5




/*
2.Given binary search tree and Integer value, remove all the nodes from the tree which have this value
Resulting tree should maintain binary search tree property.
Note: Removing a node requires rearanging the tree and not placing Leaf instead
*/

removeInt :: Int (Tree Int) -> (Tree Int)
removeInt _ Leaf = Leaf 
removeInt num (Node val tl tr) 
| num <> val = (Node val (removeInt num tl) (removeInt num tr))
| (checkNextValue num tr) = (removeInt num tr)
= (removeInt num tl)

checkNextValue :: Int (Tree Int) -> Bool
checkNextValue _ Leaf = True
checkNextValue num (Node val tl tr) 
| (num <> val) = True 
= False 

//Start = removeInt 5 (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))
// (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 6 Leaf Leaf))
//Start = removeInt 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) // Leaf



// 3.
// Given a tree, traverse it in level order.
// starting at the root element, then all elements below (left to right), then all
// elements below those (left to right), etc.
// Example:
// 1
// / \
//2  3
// / \ / \
// 4 5 6 7
// Should return [1,2,3,4,5,6,7]

traverse :: (Tree Int) -> [Int]
traverse Leaf = []
traverse (Node val tr tl) = [val] ++ traverseAux (1) (Node val tr tl)


traverseAux :: Int (Tree Int) -> [Int]
traverseAux _ Leaf = []
traverseAux lvl (Node val tr tl) 
| lvl == 1 = getChilds (Node val tr tl) ++ traverseAux (lvl + 1) (Node val tr tl)
| lvl == 2 = getChilds tr ++ getChilds tl ++ traverseAux (lvl + 1) (Node val tr tl)
| lvl == 3 = getSecondChilds tr ++ getSecondChilds tl ++ traverseAux (lvl + 1) (Node val tr tl)
= getThirdChilds tr ++ getThirdChilds tl 

getChilds :: (Tree Int) -> [Int]
getChilds Leaf = []
getChilds (Node val tr tl) = getValue tr ++ getValue tl

getSecondChilds :: (Tree Int) -> [Int]
getSecondChilds Leaf = []
getSecondChilds (Node val tr tl) = getChilds tr ++ getChilds tl

getThirdChilds :: (Tree Int) -> [Int]
getThirdChilds Leaf = []
getThirdChilds (Node val tr tl) = getSecondChilds tr ++ getSecondChilds tl

getValue :: (Tree Int) -> [Int]
getValue Leaf = []
getValue (Node val tr tl) = [val]

//Start = traverse (Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))) 
        // [15,3,20,1,10,18,21,7,13,19,26,8,11,24,28]
//Start = traverse (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) 
           // [4,3,5,3,4,5,6,2,1]