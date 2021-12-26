module code
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

bestTree :: (Tree Int)
bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
shortTree :: (Tree Int)
shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
noTree :: (Tree Int)
noTree = Leaf
unitTree :: (Tree Int)
unitTree = Node 1337 Leaf Leaf

/*
Write a function that takes a tree as an argument and counds sum of values on odd levels (depths).
lvl1:           2
              /   \
lvl2:        3     2
            / \   / \
lvl3:      1  2   3  1
First (root) element is considered to be on the odd (1st) level. It's children are on the even
level (2nd) and it's children's children are on the odd (3rd) level again and so on.
*/

oddSum :: (Tree Int) -> Int
oddSum Leaf = 0
oddSum (Node value t1 t2) = value + (sumJustOdd 2 t1 ) + (sumJustOdd 2 t2 )

sumJustOdd :: Int (Tree Int) -> Int 
sumJustOdd lvl Leaf = 0
sumJustOdd lvl (Node value t1 t2) 
| isOdd lvl = value + (sumJustOdd (nextLvl) t1) + (sumJustOdd (nextLvl) t2)
= (sumJustOdd (nextLvl) t1) + (sumJustOdd (nextLvl) t2)
where
    nextLvl = lvl + 1

//Start = oddSum bestTree // 110
//Start = oddSum ourTree // 136
//Start = oddSum shortTree // 42
//Start = oddSum unitTree // 1337
//Start = oddSum noTree // 0


/*
Write a function that takes a tree as a parameter
and returns a list of the numbers of the nodes whose children are both not Leafs.

 
notLeaves :: (Tree Int) -> [Int]
notLeaves Leaf = []
notLeaves (Node val tr tl) 
| (tr <> Leaf ) &&  (tl <> Leaf ) = [val] ++ notLeaves tr ++ notLeaves tl 
=  notLeaves tr ++ notLeaves tl 
*/


//Start = notLeaves bestTree //[10,14,17,19]
// Start = notLeaves ourTree //[15,3,10,20,26]
// Start = notLeaves shortTree //[14]
// Start = notLeaves unitTree //[]
// Start = notLeaves noTree //[]

