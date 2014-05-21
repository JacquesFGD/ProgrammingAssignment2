## MakeCacheMatrix creates a list of 4 functions from a matrix, allowing to set and get the value 
## of the matrix and its inverse. cacheSolve works on this list to return the inverted matrix, 
## either by getting it from the cache of calculating it

## Creates a list on 4 methods to access a matrix and its inverse, each method is described below

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Sets a new value for the matrix stored in the list, and deletes the saved value of inverse,
    ## both in the global environment
    set <- function(new_value) {
        x <<- new_value
        inv <<- NULL
    }
    ## Returns value of the matrix
    get <- function() x
    ## Sets function of the inverse of matrix s in the global environment
    setinv <- function(calculated_inverse) inv <<- calculated_inverse
    ## Provides value of inverse, or NULL if not yet calculated
    getinv <- function() inv

    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Tries to access the value of the inverse of the matrix stored in the list l, or calculate it
## if the value is not in cache

cacheSolve <- function(l, ...) {
    inv <- l$getinv()
    ## Return the cached value if calculated before (i.e. inv not NULL)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Accesses matrix with get and calculates inverse with solve
    data <- l$get()
    inv <- solve(data, ...)
    ## Caches inverse matrix in global variable inv
    l$setinv(inv)
    inv
}
