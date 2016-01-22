## Put comments here that give an overall description of what your
## functions do

## These functions can potentially save the computation cost of
## inverting a matrix repeatedly (as in a loop) by caching the inverse.


## Write a short comment describing this function

## makeCacheMatrix creates a list of functions that can set the value
## of the input matrix, get that matrix, set the value of the inverse
## matrix, and get that value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function

## cacheSolve first checks to see if the inverse of the matrix has
## already been calculated. If it has, it returns the message "getting
## cached data" to the console, and the value of the stored inverse
## matrix. If not, it computes and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


