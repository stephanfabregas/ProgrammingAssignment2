## Put comments here that give an overall description of what your
## functions do
## This code is based on the example given on the R Programming 
## Coursera webpage for this assignment ("Programming Assignment 2")
## and the code closely follows that for the "makeVector" and
## "cachemean" functions provided.
##
## makeCacheMatrix and cacheSolve create a special matrix-like object
## that can cache an already calculated inverse, or calculate the
## inverse if not already done and store it in the cache.
##
## For example, if you have matrix "m", you can calculate and store
## the inverse "m^(-1)" by executing v <- makeCacheMatrix(m), then 
## cacheSolve(v). The value for "m^(-1)" can now be retrieved,
## without further calculation, by calling v$getinv()

## Write a short comment describing this function
## Sets up a matrix cache that stores a matrix (x) and stores the
## inverse (NULL if not yet calculated by cacheSolve) of the matrix
## (m) rather than requiring to recalculate it every time it is
## called for
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(b) {
        x <<- b
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Makes a call to a cached matrix (x). If the inverse of the matrix
## already exists, it calls the value from the cache rather than
## recalculating, otherwise, it calculates the inverse, stores it in
## the matrix cache, and returns it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("retrieving cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
