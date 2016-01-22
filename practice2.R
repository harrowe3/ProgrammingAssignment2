## These functions can potentially save the computation cost of
## inverting a matrix repeatedly (as in a loop) by caching the inverse.

## makeCacheMatrix() takes a matrix as an argument, and creates a list of 
## functions that are used by cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve first checks to see if the inverse of the matrix has
## already been calculated in the environment created by makeCacheMatrix. 
## If that's the case, it prints "getting cached data" to the console, 
## and the value of the stored inverse matrix is returned. If not, it computes 
## and returns the inverse matrix, and caches the result using setsolve.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}