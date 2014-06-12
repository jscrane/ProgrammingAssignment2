## Put comments here that give an overall description of what your
## functions do

## constructs a special matrix which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    list(set = 
             function(y) {
                 x <<- y
                 i <<- NULL
             }, 
         get = function() x,
         setinv = function(inv) i <<- inv,
         getinv = function() i)
}

## returns the inverse of a caching matrix (see above)

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinv(i)
    i
}
