## The function 'makeCacheMatrix' create a list that caches its inverse.
## It returns a list of four functions: set, get, setinv, getinv.

makeCacheMatrix <- function(x = matrix()) {     ## Function define to use a matrix.
    inv <- NULL                                 ## Clear the last inv value   
    set <- function(y) {                        ## Set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                         ## Get the value of the matrix
    setinv <- function(z) inv <<- z             ## Set the value of the inverse
    getinv <- function() inv                    ## Get the value of the inverse
        list(set = set,                         ## List containing the functions
             get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## The function 'cacheSolve'return the inverse of the special "matrix" returned 
## by 'makeCacheMatrix' above. Return the inverse of the original matrix.

cacheSolve <- function(x, ...) { 
    inv <- x$getinv()                           ## Search the cached value for the inverse
    if (!is.null(inv)) {                        ## If the inverse has already been calculated
        message("getting cached data")          ## Get it from the cache and skips the computation
        return(inv)                                
    }
    data <- x$get()                             ## If not, it calculates the inverse
    inv <- solve(data, ...)
    x$setinv(inv)                               ## It sets the value of the inverse in the cache.
    inv                                         ## Return the inverse
}
