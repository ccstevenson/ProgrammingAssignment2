## This module calculates the inverse of a matrix and caches that inverse after the initial calculation.
## This can dramatically improve the performance of repeated retrievals of the inverse, particularly if the matrix is large.

## makeCacheMatrix is a wrapper around a matrix object.
## This wrapper holds a matrix and a getter/setter for its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) {
  myInverse <- NULL
  set <- function(newMatrix) {
    myMatrix <<- newMatrix
    myInverse <<- NULL # The inverse for the new matrix has not yet been calculated.
  }
  get <- function() myMatrix
  setInverse <- function(calculatedInverse) myInverse <<- calculatedInverse
  getInverse <- function() myInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inverse of myMatrix if it has already been calculated.
## Otherwise, it calculates the inverse and caches it in the makeCacheMatrix's setInverse property.
cacheSolve <- function(myMatrix, ...) {
  currentInverse <- myMatrix$getInverse()
  if(!is.null(currentInverse)) { # If the inverse has already been calculated . . . 
    message("getting cached data")
    return(currentInverse) # return the prior calculation.
  }
  
  # Perform the calculation and cache the result before returning it.
  data <- myMatrix$get()
  currentInverse <- solve(data, ...)
  myMatrix$setInverse(currentInverse)
  currentInverse 
}

# Example usage:
# m <- makeCacheMatrix(matrix(c(3, -7, 5, 2), nrow=2, ncol=2))
# print(m$get())
# cacheSolve(m)
# print(m$getInverse())
# print(m$getInverse())