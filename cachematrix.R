## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix and gives some methods to access data related to the matrix such as getting or setting the value of the matrix or getting or setting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function compute the inverse of a square matrix that was previously created with the makeCacheMatrix function. It either calculate the inverse using the solve() built-in function and cache it or retrieve the cached inverse if the inverse was already calculated.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
