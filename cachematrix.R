## makecacheMatrix & cacheSolve work in tandem to provide an inverse
## for well-formed square matrices. No error checking is done.

## Sets up a "CLASS" to handle caching of square matrix inversing
## Retuens list of functions for a matrix xparent

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## if a cache inverse exists for input matrix returns it
## if no cache solution exists then creates an inverse, stores it and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    #print(m)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    #print(m)
    x$setsolve(m)
    m
}
