## These functions create a cache for an invertible matrix in order to simplify the process
## of matrix inversion if the inverse has already been computed.

## This function creates a list that sets the matrix, gets the matrix, sets the inverse, and
## gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function(){x}
        setinv <- function(inv) {i <<- inv}
        getinv <- function() {i}
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function returns the inverse of a matrix, but does it from the cache if 
## the inverse was previously computed.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinv(i)
        i
}
