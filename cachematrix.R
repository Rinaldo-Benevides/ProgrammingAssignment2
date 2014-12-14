## The cacheMatrix file contains two functions.
## The makeCacheMatrix creates a "special matrix" object 
## that can cache the input matrix and its inverse.
## The cacheSolve calculates inverse matrix of "special matrix" object.


## makeCacheMatrix:
## Creates an object that contains the original 
## matrix and its inverse when it is calculated 

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(newValue) {
        
        # I want to make sure new value is of type "matrix"
        if(!is.matrix(newValue)) stop("New value must be a matrix")
        
        x <<- newValue
        
        inverse <<- NULL
    }
    
    get <- function() x ## Return Matrix
    
    setInverse <- function(newValue) inverse <<- newValue
    
    getInverse <- function() inverse ## Return Inverse Matrix
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve:
## calculates inverse matrix of "special matrix" object
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache. If the input is new, 
## it calculates the inverse and sets in the cache.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("Getting cached inverse matrix")
        return(inverse)
    }
    
    matrix <- x$get()
    
    inverse <- solve(matrix, ...)
    
    x$setInverse(inverse)
    
    inverse
}
