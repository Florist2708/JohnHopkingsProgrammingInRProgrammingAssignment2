## These functions create a special matrix object that can cache its
## inverse so repeated calls do not recompute solve(x) unnecessarily.

## Create a matrix object with methods to set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

## Return the cached inverse if available; otherwise compute and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
