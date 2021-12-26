module code
import StdEnv


evenSum :: [Int] -> Int
evenSum lst = sum(filter isEven lst)

//Start = evenSum [12,4,5,7]

sqr x = x * x
squareSum :: Int -> Int
squareSum num = sum (map sqr (filter isEven  [1,2 .. num]))

//Start = squareSum 10

oddElevens :: Int -> [Int]
oddElevens num  = [11, 22 .. 11 * num]

//Start = oddElevens 10

f :: [Int] -> [Int]
f list = filter (\x = (x rem 10 == 0 && x rem 4 <> 0) ) list 

Start = f [10, 20 .. 150 ]
