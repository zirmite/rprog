## These two functions create and calculate (and cache) the 
## inverse of a square matrix

## The makeCacheMatrix function takes a matrix and wraps it 
## in code to allow the inverse to be cached for future use

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv1) inv <<- inv1
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function takes the wrapped matrix created using 
## makeCacheMatrix and returns the inverse, calculating it if necessary
## i.e., if it is being called for the first time on this particular matrix 
## instance.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
