## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object  
## that stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseM) invM <<- inverseM
    getInverseMatrix <- function() invM
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invM <- x$getInverseMatrix()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setInverseMatrix(invM)
    invM
}
