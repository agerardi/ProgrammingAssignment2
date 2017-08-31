## The functions implement a kind of matrix object and
## an associated method to calculate the inverse of the matrix
## optimized to retrieve the inverse matrix from memory in case
## it has already been calculated before

## Implementation of the matrix object along with the four methods
## to get and set the matrix and its inverse:

makeCacheMatrix <- function(x = matrix()) {
     # Initialize the inverse matrix to NULL:
     inv_mat <- NULL
	 # Create get and set methods:
     set <- function(m) {
          x <<- m
          inv_mat <<- NULL
     }
     get <- function() x
     setinv <- function(inv) inv_mat <<- inv
     getinv <- function() inv_mat
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)     
}

## Implementation of the method that returns the 
## inverse of the matrix x:

cacheSolve <- function(x, ...) {
     # Access to the inverse matrix stored in the object:
     inv_mat <- x$getinv()
     # If it has a value not null, return it and exit:
     if(!is.null(inv_mat)) {
          message("getting cached data")
          return(inv_mat)
     }
     # Otherwise, compute the inverse matrix, store it in the object and return its value: 
     data <- x$get()
     inv_mat <- solve(data, ...)
     x$setinv(inv_mat)
     inv_mat
}