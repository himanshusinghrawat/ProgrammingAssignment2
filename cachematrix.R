## Inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
            x <<- y
            xinv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xinv <<- inverse
    getInverse <- function() xinv
    list(set = set,get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


##This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already
##been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xinv <- x$getInverse()
    if (!is.null(xinv)) {
         message("getting cached data")
         return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setInverse(xinv)
    xinv
}
