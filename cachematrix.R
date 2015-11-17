## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverted <<- inverse
        getInverse <- function() inverted
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computes the inverse of the matrix created by makeCacheMatrix.
## If already calculated --> it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverted <- x$getInverse()
        if (!is.null(inverted)) {
                message("Back in a sec... Getting the data from the cache")
                return(inverted)
        }
        matrix <- x$get()
        inverted <- solve(matrix, ...)
        x$setInverse(inverted)
        inverted
}