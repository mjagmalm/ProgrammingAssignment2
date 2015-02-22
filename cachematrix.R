
## makeCacheMatrix and cacheSolve are two functions for making matrix invertion handling more efficient. 
## Use makeCacheMatrix to store data about a matrix. cacheSolve is used to retreive the inverse of a matrix. It uses a cached version if one is available.

## This function takes a matrix as input parameter.
## The function returns a list with functions to get and set the matrix, and to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  get = function() x 
  set = function(y) x = y
  getInverse = function() return(inv)
  setInverse = function(i) inv <<- i
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

## This function takes a matrix as input parameters, and extra parameters to the solve function
## The function returns the inverse of the matrix. If a cached version exists the cached version is returned. Otherwise the inverse of the matrix is calculated.

cacheSolve <- function(x, ...) {
  inv = x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }  
  inv = solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
