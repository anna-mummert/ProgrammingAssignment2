## A pair of functions that cache the inverse of an invertible matrix.  

## makeCacheMatrix holds the matrix and its cached inverse.  
# The function also allows the matrix to be seen (get) and redefined (set).  

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(m) matrix_inverse <<- m
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## cacheSolve displays the inverse of an invertible matrix.  
# If the matrix has a cached inverse, then the function pulls the inverse from memory.
# Otherwise, the function computes the inverse and stores it using the "setinverse" 
# function that is part of makeCacheMatrix.  

cacheSolve <- function(x, ...) {
        matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        my_matrix <- x$get()
        matrix_inverse <- solve(my_matrix, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}
