## Programming Assignment 2: calculate and cache the inversion of a matrix
## Assumption: all the matrices passed in are invertible

## Create a new matrix pbject and cache the inversion
makeCacheMatrix <- function(x = matrix()) {
  # cached value of the inverse
  inverse <- NULL

  # getter/setter for matrix, on setting a new matrix, nullify the inverse
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function() x
  
  # getter/setter for inverse
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)
}

## Compute the inversion returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # get cached inverse
  inverse <- x$getinverse()
  
  # If it's there, calculate and save the inverse
  if (is.null(inverse)) {
    inverse <- solve(x$getmatrix(), ...)
    x$setinverse(inverse)
  }
  
  # return inverse
  inverse
}
