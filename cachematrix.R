#### Project: Project 2 Coursera 
#### Course: "R Programming"
#### Author: Costa, S. 
#### Date: May 10, 2014



## Example of use / implementation of tasks: 
## > x <- matrix(rnorm(4), nrow = 2)          
## > xx <- makeCacheMatrix(x) 
## > cacheSolve(xx)
## > cacheSolve(xx) 


## The makeCacheMatrix creates a matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the inverse is already calculated, it will instead find it in the cache 
## and return a inverse matrix object (not calculate it again).
makeCacheMatrix <- function(x = numeric()) {
    ## inverse matrix
    invMatrix <- NULL
    
    ## set matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    ## get matrix
    get <- function() x
    
    ## set inverse
    invMatrixSet <- function(inverse) invMatrix <<- inverse
    ## get inverse
    invMatrixGet <- function() invMatrix
    
    # Return the matrix 
    list(set = set, get = get, invMatrixSet = invMatrixSet, invMatrixGet = invMatrixGet)
}

## The function cacheSolve returns the inverse of a matrix 'X' 
## created with the makeCacheMatrix function.
## If the inverse is already calculated before, it returns the cached inverse.
## While if the inverse isnÂ´t yet calculated before, 
## it computes, caches and returns the inverse.
cacheSolve <- function(x, ...) {
    invMatrix <- x$invMatrixGet()
    
    # If the inverse is already calculated:: return inverse
    if (!is.null(invMatrix)) {
        message("Getting Cached Inverse Matrix")
        return(invMatrix)
    }
    
    # The inverse isn't yet calculated:: calculate inverse
    data <- x$get()
    invMatrix <- solve(data, ...)
    
    # Cache the inverse
    x$invMatrixSet(invMatrix)
        
    ## Return a matrix that is the inverse of 'x'
    invMatrix
}