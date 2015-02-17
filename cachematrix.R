## R Programming - Programming assignment 2
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly
##
## This pair of functions will cache the inverse of a matrix

##Function to create the cacheable matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
    ## at creation of the cache set the inverse null
    i <- NULL
    ## create the "set" function that will set the value of the matrix
    ## and set the inverse null, because we haven't solved it when assigned
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    ## create the "get" function to return the matrix
    get <- function() x
    ## create the "setinverse" function to cache the inverse
    setinverse <- function(inverse) i <<- inverse
    ## create the "getinverse" function to return the inverse
    getinverse <- function() i
    ## return the list of functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function to Return a matrix that is the inverse of 'x'
## Will check if the inverse is already solved, if so use it
## or else solve and cache the result
cacheSolve <- function(x, ...) {
    
    ## get the inverse from the cached matrix
    i <- x$getinverse()
    ## if it is not null return it
    if(!is.null(i)){
      message("getting cached inverse")
      return(i)
    }
    ## it was null, get the matrix
    mat <- x$get()
    ## solve the matrix
    i <- solve(mat, ...)
    ## cache the inverse
    x$setinverse(i)
    ## return the result
    i
}
