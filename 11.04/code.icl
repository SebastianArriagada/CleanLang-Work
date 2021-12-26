module code 
import StdEnv

:: Tree a = Node a (Tree a) (Tree a)
            | Leaf

// Count nodes 

countNodes :: ( Tree a) -> Int 
countNodes Leaf = 0
countNodes ( Node val tl tr) = 1 + (countNodes tl) + (countNodes tr)

tree1 = (Node 1 ( Node 2 Leaf Leaf) Leaf)
tree2 = ( Node 3 tree1 (Node 3 tree1 tree1))

//Start = countNodes tree2

treeMap :: (Int -> Int) (Tree Int) -> (Tree Int)
treeMap f Leaf = Leaf
treeMap f ( Node val tl tr ) = (Node (f val) (treeMap f tl) (treeMap f tr))

//Start = treeMap inc tree1 

collect :: ( Tree a ) -> [a]
collect Leaf = []
collect (Node val tl tr) = (collect tl) ++ [val] ++ (collect tr)

listToTree :: [Int] -> Tree Int
listToTree [] = Leaf
listToTree [x:xs] = insertNode x (listToTree xs)

insertNode :: Int ( Tree Int) -> (Tree Int)
insertNode newVal Leaf = (Node newVal Leaf Leaf)
insertNode newVal (Node val tl tr)
| newVal < val = (Node val (insertNode newVal tl) tr)
= (Node val tl (insertNode newVal tr))

//Start listToTree [2,4,3,8,1]