## Put comments here that give an overall description of what your
## functions do

##Wraps a matrix with an object that can store its inverse.
##The inverse should be provided by calling functions.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ##set wrapped matrix and reinit stored inverse matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ##return wrapped matrix
    get <- function() x
    
    ##store inverse matrix
    setInverse <- function(inv) inverse <<- inv
    ##retrieve inverse matrix, could be null.
    getInverse <- function() inverse
    #export functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##Retrieve the inverse stored in a cached matrix object (see makeCacheMatrix).
##If not computed yet, compute and store it.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    ##compute and store inverse matrix
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
