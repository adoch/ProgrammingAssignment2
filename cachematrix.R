## The below two functions are used in order to improve the 
## performance of the calculation of the inverse of a matrix by caching it.
## Using them, the inverse of a matrix is only calculated the first time
## that it will be asked and at the same time is cached for any future reference.

## The makeCacheMatrix function creates a special matrix and offers a list of 
## functions to return the matrix itself and the inverse of it 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(inv) m <<- inv
    getMatrixInverse <- function() m
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## The cacheSolve function returns the inverse matrix of a matrix 
## that is "created" using the makeCacheMatrix function. If the inverse 
## matrix has already been asked before then it is retrieved from the cache
## otherwise it is calculated and cached for future reference

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrixInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrixInverse(m)
    m
}