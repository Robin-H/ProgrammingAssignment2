## Create a special wrapper around a matrix with the makeCacheMatrix. Use this
## this wrapper in turn as an argument for cacheSolve to get the inverse of the
## previously supplied matrix. cacheSolve always tries to obtain the inverse 
## from the cache. If not available, it calcultates the inverse and stores it in
## the cache for future use.

## Create a wrapper around a matrix for usage with the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Get the cached inverse of a matrix within the makeCacheMatrix wrapper. If the
## cache is empty, this function calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Using cached inverse")
                return(inv)
        }
        # No cache found, calculate inverse, set cache and return inverse
        rawMatrix <- x$get()
        inv <- solve(rawMatrix, ...)
        x$setInverse(inv)
        inv
}
