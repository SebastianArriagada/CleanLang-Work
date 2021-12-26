module test
import StdEnv



:: Major = CS | Math | Physics | Linguistics
:: Course = {name::String, major :: Major, credits:: Int, max_student:: Int}

Programming::Course
Programming = {name="Programming",major=CS, credits =5, max_student=50}

Functional::Course
Functional = {name="Functional",major=CS,credits=5, max_student=120}

Imperative::Course
Imperative = {name="Imperative",major=CS,credits=4, max_student=150}

Analysis::Course
Analysis = {name="Analysis",major=Math, credits =4, max_student=85}

BasicMath::Course
BasicMath = {name="BasicMath",major=Math,credits=2, max_student=40}

Hungarian::Course
Hungarian = {name="Hungarian",major=Linguistics,credits=3, max_student=20}

maximumSubarray :: {Int} Int -> {Int}
maximumSubarray arr num =  { y \\ y<- snd (splitAt (length [ x \\ x<-:arr ] - 2) (sort [ x \\ x<-:arr ]))}

Start = maximumSubarray {1, 2, -2, -1, -5, 10, 4} 2	