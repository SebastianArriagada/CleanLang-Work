module code
import StdEnv

toArray :: [Int] -> {Int}
toArray lst = {a \\ a<-lst}


Start = toArray [4,5,2,3,4]
