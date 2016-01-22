## assignment2_clarifying_instr.R

##function plusFunctions stores two functions:
## plustwo() : sums 2 to the given value
## plusthree() : sums 3 to the given value
plusFunctions <- function (){
        plustwo <- function(y) {
                x <- y + 2
                return(x)
        }
        plusthree <- function(y) {
                x <- y + 3
                return(x)
        }
        #the following line stores the two functions:
        list(plustwo = plustwo, plusthree = plusthree) 
}
