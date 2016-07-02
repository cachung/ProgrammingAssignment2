## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initial the inv
    inv <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function()
        x
    
    ## Set the value of the inverse
    setInverse <- function(inverse)
        inv <<- inverse
    
    ## Get the value of the inverse
    getInverse <- function()
        inv
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mymatrix <- x$get()
    ## Call solve(c) %*% c to calculate the inverse
    
    inv <- solve(mymatrix, ...)
    x$setInverse(inv)
    inv
}
