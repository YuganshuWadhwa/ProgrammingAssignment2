## This R script calculates the inverse of a matrix and stores it in cache memory
## to save computational time when the same matrix is passed again to the functions
## to find the inverse.

## This function creates a matrix and caches it.
## The member function setinv caches the calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function 1st checks if the matrix inverse already exists in cache.
## If yes then the inverse in cached and returned.
## If not then the inverse is computed and returned.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    if( !is.null(inv) ) {
        message("Getting Cached Data")
        return(inv)
    }
  
    data <- x$get()
    
    inv <- solve(data, ...)
    x$setinv(inv)
    
    inv
}
